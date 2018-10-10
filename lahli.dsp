
import("stdfaust.lib");
      // slidingMinN(n,zzz) = min ~ *(1.0-(1.0/max(1.0,n)));
      import("../LazyLeveler/slidingReduce.lib");

      maxHoldTime = pow(2,13);
      SampleRate = 44800;

      maxHoldMs   = maxHoldTime*1000/SampleRate;
      holdTime    = int((hslider("[0]maximum hold time[unit:ms] [tooltip: maximum hold time in ms][scale:log]", maxHoldMs, 0.1, maxHoldMs ,0.1))/1000*SampleRate);


      process(x) = (x*inGain)@maxHoldTime*gain(x*inGain);

      gain(x) = ba.db2linear(dBgain(x):meter);
      dBgain(x) =
        ((((now > futuredown) * (min(now,_)))
        +
        ((now <= futuredown) * now))~_)
        with {
            now = currentdown(x)@maxHoldTime;
            futuredown = currentdown(x):slidingMinN(holdTime,maxHoldTime)@max(0,(maxHoldTime - holdTime));
        };
      GRmeter_group(x)  = (hgroup("[3] GR [tooltip: gain reduction in dB]", x));
      meter           = GRmeter_group(  _<:(_,(_:min(0):max(-20):( (vbargraph("[unit:dB]", -20, 0))))):attach);
      currentLevel(x)     = ((abs(x)):ba.linear2db);

      currentdown(x)      = 0-(((currentLevel(x))-(threshold)):max(0));


      inGain                  = (hslider("[0]input gain [unit:dB]   [tooltip: input gain in dB ", 0, 0, 30, 0.1)):si.smooth(0.999):ba.db2linear ;
      threshold               = (hslider("[1]threshold [unit:dB]   [tooltip: maximum output level in dB]", -0.5, -60, 0, 0.1):si.smooth(0.999));
