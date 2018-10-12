
import("stdfaust.lib");
      // slidingMinN(n,zzz) = min ~ *(1.0-(1.0/max(1.0,n)));
      import("../LazyLeveler/slidingReduce.lib");

      // maxHoldTime = 8;
      maxHoldTime = pow(2,13);
      maxAttackTime = 64;

      SampleRate = 44100;

      maxHoldMs   = maxHoldTime*1000/SampleRate;
      holdTime    = int((hslider("[0]maximum hold time[unit:ms] [tooltip: maximum hold time in ms][scale:log]", maxHoldMs, 0.1, maxHoldMs ,0.1))/1000*SampleRate);
      // todo: attack knee
      // attack is when gr(knee)<prevGR(knee)

      process(x,y) =
      l@latency * lGain
      ,
      r@latency * rGain
      ,
      lGain
      with {
          l = x*inGain;
          r = y*inGain;
          latency = maxHoldTime+maxAttackTime;
          stereoGain = min(dBgain(l),dBgain(r));
          lGain = dBgain(l) , stereoGain : linearXfade(link):release:meter:ba.db2linear;
          rGain = dBgain(r) , stereoGain : linearXfade(link):release:meter:ba.db2linear;
      };

      linearXfade(x,a,b) = a*(1-x),b*x : +;

      dBgain(x) =
            ((
                ((now > futuredown) * (min(now,_)))
                +
                ((now <= futuredown) * now)
                ) )~_
        with {
            now = currentdown(x)@(maxHoldTime);
            futuredown = currentdown(x):slidingMinN(holdTime,maxHoldTime)@max(0,(maxHoldTime - holdTime ));
        };
      currentLevel(x)     = ((abs(x)):ba.linear2db);

      currentdown(x)      =
        par(i, maxAttackTime, (gain_computer(threshold,knee,currentLevel(x))@i)/(maxAttackTime-i) ) : blockmin(maxAttackTime);

      blockmin(2) = min;
      blockmin(n) = blockmin(n/2),blockmin(n/2):min;


      release(GR) = linRelease(GR); // todo: make xfade between lin and log
      linRelease(GR) = //GR;
        (
          isAttack(_)*GR
          +
          isRelease(_)*release_slew(_)
        )~(_<:(si.bus(4)))
        with {
            isAttack(prevGR) = GR<=prevGR;
            isRelease(prevGR) = GR>prevGR;
            release_slew(prevGR) = prevGR - max(0-release_dB,( prevGR-GR ));
        };


      gain_computer(thresh,knee,level) =
      select3((level>(thresh-(knee/2)))+(level>(thresh+(knee/2))),
                                        0,
                                        ((level-thresh+(knee/2)):pow(2)/(2*knee)) ,
                                        (level-thresh)
             ) :max(0) *-1;

      inGain                  = (hslider("[0]input gain [unit:dB]   [tooltip: input gain in dB ", 0, 0, 30, 0.1)):si.smooth(0.999):ba.db2linear ;
      threshold               = (hslider("[1]threshold [unit:dB]   [tooltip: maximum output level in dB]", -0.5, -60, 0, 0.1):si.smooth(0.999));
      GRmeter_group(x)  = (hgroup("[3] GR [tooltip: gain reduction in dB]", x));
      release_dB        = hslider("[0]release rate[unit:dB/s][tooltip: release rate when the GR is at AVG, in dB/s][scale:log]", 30, 6, 400 , 0.1)/SampleRate;
      meter           = GRmeter_group(  _<:(_,(_:min(0):max(-20):( (vbargraph("[unit:dB]", -20, 0))))):attach);
      knee = (hslider("[2] Knee [unit:dB] [tooltip: soft knee amount in dB]", 6, 0, 30, 0.1));
      link              = (hslider("[5]stereo link[tooltip: 0 means independent, 1 fully linked]", 1, 0, 1 , 0.001));
