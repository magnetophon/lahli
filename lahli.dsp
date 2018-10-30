      import("stdfaust.lib");
      import("../LazyLeveler/slidingReduce.lib");

      // todo: make attack and hold SR dependant
      // Subject: Re: [Faudiostream-users] Status of control/enable primitives and the
      // -es compiler option
      // Date: wo 24 okt 2018 19:32:15 CEST

      // maxHoldTime = 4;
      maxHoldTime = pow(2,12);
      maxAttackTime = 512;

      SampleRate = 44100;

      maxHoldMs   = maxHoldTime*1000/SampleRate;

      process = limiter;

      mbTest =
      si.bus(1)<:
      ( (
              fi.filterbank(5,(par(i, 3, fx(i))))
              // fi.mth_octave_filterbank5(1,fx,4)
            // :limiter_N_chan(4)
           : par(i, 4, limiter_N_chan(1))
           :>_)
                   // ,_//(_*-1)
                   :limiter_N_chan(1)
                   )//:>_
      // fi.low_shelf
      ;
      fx(i) = hslider("fx%i", 1000, 20, 20000, 1);

      limiter =
      si.bus(2)<:si.bus(4):
      limiter_N_chan(2),
      (par(i, 2, _*inGain ) : limiter_gain_N_chan(2): par(i, 2, release:ba.db2linear))
      ;


      // generalise limiter gains for N channels.
      // first we define a mono version:
      limiter_gain_N_chan(1) =
      dBgain;

      // The actual N-channel version:
      // Calculate the maximum gain reduction of N channels,
      // and then crossfade between that and each channel's own gain reduction,
      // to link/unlink channels
      limiter_gain_N_chan(N) =
      par(i, N, dBgain)
      <:(si.bus(N),(minimum(N)<:si.bus(N))):ro.interleave(N,2):par(i,N,(linearXfade(link)));


      latency = maxHoldTime+maxAttackTime:hbargraph("latency [lv2:reportsLatency] [lv2:integer] [unit:frames]", maxHoldTime+maxAttackTime, maxHoldTime+maxAttackTime);

      limiter_N_chan(1) = _*inGain <:((dBgain:release:meter:ba.db2linear)*_@latency);

      limiter_N_chan(N) =
      (par(i,N,_*inGain )<:
         (limiter_gain_N_chan(N),si.bus(N))
      )
      :(ro.interleave(N,2):par(i,N,(release:meter:ba.db2linear)*(_@latency)));

      linearXfade(x,a,b) = a*(1-x),b*x : +;

      dBgain(x) =
      (
          ((now >  futuredown) * (min(now,_)))
          +
          ((now <= futuredown) * now)
      )~_
        with {
          now = currentdown(x)@(maxHoldTime);
          futuredown = currentdown(x):slidingMinN(holdTime,maxHoldTime)@max(0,(maxHoldTime - holdTime ));
        };

      currentLevel(x)     = ((abs(x)):ba.linear2db);

      currentdown(x) =
      par(i, maxAttackTime+1, (gain_computer(threshold,knee,currentLevel(x))@i)*(((i+1)/(maxAttackTime+1)) : attackShaper ) ) : minimum(maxAttackTime+1);

      attackShaper(fraction)= ma.tanh(fraction:pow(attack:attackScale)*(attack*5+.1))/ma.tanh(attack*5+.1);
      attackScale(x) = (x+1):pow(7); //from 0-1 to 1-128, just to make the knob fit the aural experience better
      attack                  = (hslider("[2]attack shape[tooltip: 0 gives a linear attack (slow), 1 a strongly exponential one (fast)]", 1 , 0, 1 , 0.001));
      // get the minimum of N inputs:
      minimum(1) = _;
      minimum(2) = min;
      minimum(N) = (minimum(N-1),_):min;



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
