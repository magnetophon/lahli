
      import("stdfaust.lib");
      import("../LazyLeveler/slidingReduce.lib");

      maxHoldTime = pow(2,12);
      maxAttackTime = 64;

      SampleRate = 44100;

      maxHoldMs   = maxHoldTime*1000/SampleRate;

      process(x,y) = limiter(x,y);

      limiter(x,y) =
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

      RMS(time) = slidingRMSn(time,maxRMStime);


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



      release(GR) = GR : si.lag_ud(release_ms,0);

      gain_computer(thresh,knee,level) =
      select3((level>(thresh-(knee/2)))+(level>(thresh+(knee/2))),
                                        0,
                                        ((level-thresh+(knee/2)):pow(2)/(2*knee)) ,
                                        (level-thresh)
             ) :max(0) *-1;

      mainGroup(x)        = hgroup("[0]", x);
      knobGroup(x)     = mainGroup(vgroup("[0]", x));
      inGain           = knobGroup(hslider("[0]input gain [unit:dB]   [tooltip: input gain in dB ", 0, 0, 30, 0.1)):si.smooth(0.999):ba.db2linear ;
      threshold        = knobGroup(hslider("[1]threshold [unit:dB]   [tooltip: maximum output level in dB]", 0.0, -60, 0, 0.1):si.smooth(0.999));
      release_ms       = knobGroup(hslider("[2] release [unit:ms] [scale:log]
        [tooltip: Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')]",
        20, 1, 500, 0.1)) : *(0.001) : max(1/ma.SR);
      holdTime         = int(knobGroup(hslider("[3]hold time[unit:ms] [tooltip: hold time in ms][scale:log]", 50, 0.1, maxHoldMs ,0.1))/1000*SampleRate);
      knee             = knobGroup(hslider("[4] knee [unit:dB] [tooltip: soft knee amount in dB]", 0, 0, 30, 0.1));
      link             = knobGroup(hslider("[5]stereo link[tooltip: 0 means independent, 1 fully linked]", 1, 0, 1 , 0.001));

      meterGroup(x)    = mainGroup(vgroup("[1]", x));
      GRmeter_group(x) = meterGroup(hgroup("[0] GR [tooltip: gain reduction in dB]", x));
      meter            = GRmeter_group(  _<:(_,(_:min(0):max(-20):( (vbargraph("[unit:dB]", -20, 0))))):attach);
