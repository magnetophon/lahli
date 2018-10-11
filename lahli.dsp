
import("stdfaust.lib");
      // slidingMinN(n,zzz) = min ~ *(1.0-(1.0/max(1.0,n)));
      import("../LazyLeveler/slidingReduce.lib");

      // maxHoldTime = 4;
      maxHoldTime = pow(2,13);

      SampleRate = 44800;

      maxHoldMs   = maxHoldTime*1000/SampleRate;
      holdTime    = int((hslider("[0]maximum hold time[unit:ms] [tooltip: maximum hold time in ms][scale:log]", maxHoldMs, 0.1, maxHoldMs ,0.1))/1000*SampleRate);


      process(x) =
        (x*inGain)@maxHoldTime*gain(x*inGain)
        ,gain(x*inGain);

      gain(x) = ba.db2linear(dBgain(x):meter);
      dBgain(x) =
            ((
                ((now > futuredown) * (min(now,_)))
                +
                ((now <= futuredown) * now)
                ) )~_ :release
        with {
            now = currentdown(x)@maxHoldTime;
            futuredown = currentdown(x):slidingMinN(holdTime,maxHoldTime)@max(0,(maxHoldTime - holdTime));
        };
      currentLevel(x)     = ((abs(x)):ba.linear2db);

      currentdown(x)      = 0-(((currentLevel(x))-(threshold)):max(0));

      release(GR) = //GR;
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



      inGain                  = (hslider("[0]input gain [unit:dB]   [tooltip: input gain in dB ", 0, 0, 30, 0.1)):si.smooth(0.999):ba.db2linear ;
      threshold               = (hslider("[1]threshold [unit:dB]   [tooltip: maximum output level in dB]", -0.5, -60, 0, 0.1):si.smooth(0.999));
      GRmeter_group(x)  = (hgroup("[3] GR [tooltip: gain reduction in dB]", x));
      release_dB        = hslider("[0]release rate[unit:dB/s][tooltip: release rate when the GR is at AVG, in dB/s][scale:log]", 30, 6, 200 , 0.1)/SampleRate;
      meter           = GRmeter_group(  _<:(_,(_:min(0):max(-20):( (vbargraph("[unit:dB]", -20, 0))))):attach);
