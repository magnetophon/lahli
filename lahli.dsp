import("stdfaust.lib");
import("../LazyLeveler/slidingReduce.lib");

// todo: make attack and hold SR dependant
// Subject: Re: [Faudiostream-users] Status of control/enable primitives and the
// -es compiler option
// Date: wo 24 okt 2018 19:32:15 CEST

// todo:
// attack =
// 1) find nr of samples between changes
// 2) shorten attack to nr of samples in 1) with a max of maxAttackTime
// 3) trigger ramp
// 4) use ramp for attackShaper

// * two blocks of attack-delay: one for analysys, one for gain:
// ** in analysis block:
// *** at change, start counting from 0 to maxAttackTime
// ** in the gain block:

// make an array with:
// - startGR
// - length
// gain = (attackRamp@maxAttackTime * ( endGR - startGR )) + startGR
// attackGain =

clock = ba.time;
wrap(wr,val) = ((val%wr)+wr)%wr;

attackRel(x) =
  (linearXfade( attackCtrl, startGR, endGR ))
  // ,
  // x@maxAttackTime,
  // startGR,
  // endGR
with {
  length =  rwtable(size+2, maxAttackTime, windex , attackCount' , (Lrindex+1):wrap(size)@maxAttackTime):max(1):min(maxAttackTime);

  length1 =  rwtable(size+2, maxAttackTime, windex , attackCount' , (Lrindex+1):wrap(size)@maxAttackTime);

  changetime =  rwtable(size+2, 0, windex , clock , Lrindex);

  ramp = select2( (x@maxAttackTime)==endGR1, (((clock-changetime)@maxAttackTime)-rampOffset):max(0) ,0 );
  attackCtrl = select2( startGR>endGR, 0, (ramp/length) ) :attackShaper;
  // ramp = (((clock-changetime)@maxAttackTime)-rampOffset):max(0);
  rampOffset = (length1 - maxAttackTime):max(0);


  // length1 = rwtable(size+2, maxAttackTime, windex, attackCount' , Lrindex1);
  // endGR   = startGR@length;

  endGR1   = rwtable(size+2, 0.0, windex, x, GRrindex );
  // endGR =  rwtable(size+2, 0.0, windex , x , (Lrindex+1):wrap(size));


  // endGR   = rwtable(size+2, 0.0, windex, x', (GRrindex-1):wrap(size) );

  // startGR = rwtable(size+2, 0.0, windex, x,  (GRrindex-1):wrap(size) )@maxAttackTime;
  // startGR = rwtable(size+2, 0.0, windex, x, GRrindex );

  // theEnd =   rwtable(size+2, 0.0, windex, x,  (GRrindex-1):wrap(size) )@maxAttackTime;
  endGR =  rwtable(size+2, 0.0, windex , x , (Lrindex+1):wrap(size)@maxAttackTime);
  startGR = rwtable(size+2, 0.0, windex, x, (GRrindex):wrap(size) )@maxAttackTime;

  // startGR = rwtable(size+2, 0.0, windex, x',  GRrindex)@(maxAttackTime-1);

  // endGR   = rwtable(size+2, 0.0, windex, x, GRrindex);
  // startGR = rwtable(size+2, 0.0, windex, x, (((GRrindex-1)%size)+size)%size );
  windex = select2(x==x', windexCount, size+1);
  windexCount = (_<:select2(x==x',_+1,_) % size)~_;
  GRrindex = windexCount;
  Lrindex1 = (windexCount+1):wrap(size);
  Lrindex = windexCount;
  attackRamp(x) = // ramp from 0 to 1 in the time between 2 attacks:
    ((attackCount@2) / length) : attackShaper; // latency is 2
  attackCount =
    // select2(x==x',0,_+1:min(maxAttackTime))~_;
  select2(x==x',0,_+1)~_;
  size = maxAttackTime;
};

// maxHoldTime = 4;
maxHoldTime = pow(2,12);
// maxAttackTime = 512;
maxAttackTime = 2048;
// maxAttackTime = 64;

SampleRate = 44100;

maxHoldMs   = maxHoldTime*1000/SampleRate;

// process = control(os.osc(440), choice==0), control(os.osc(660), choice==1)
// :> _;

lim = control(limiter(2), choice==0), control(si.bus(2)@latency, choice==1)
                                      :> si.bus(2)
    ;
    choice  = button("choice");


    // process(x) =
      // attackRel((x:min(0.999):max(0.001)))
      // :release
      // ,x@maxAttackTime;
    process =
    limiter(2);
    // (_<:select2(x==x',_+1,_) % size)~_;
    // samplesBetweenChange;



    attackGain2(ramp,length, x) =
      ((x-prev)*ramp)+prev
    with {
      prev = x: ba.sAndH(ramp==0);
    };

    // attackRamp(x) = // ramp from 0 to 1 in the time between 2 attacks:
    // ((attackCount@2) / samplesBetweenChange(x)) : attackShaper; // latency is 2



    // sAndH: (0 for hold, 1 for bypass)
    samplesBetweenChange(x) =  attackCount' : ba.sAndH(x==x'); // latency is 1

    attackGRrelative =
      (0: seq(i,maxAttackTime,
              // ((lastRel(i)+((currentdown(x)@(i+1-maxAttackTime+maxHoldTime))*(((1)/maxAttackTime)))):(_<:((_<lastdown),_):*)),_: min
              // ((((currentdown(x)@(i+1-maxAttackTime+maxHoldTime)-lastRel(i))*(((i+1)/maxAttackTime):attackShaper))+lastRel(i)):(_<:((_<lastdown),_):*)),_: min
              ((((currentdown(x)@(i+1-maxAttackTime+maxHoldTime)-lastdown)*(((i+1)/maxAttackTime):attackShaper))+lastdown):(_<:((_<lastdown),_):*)),_: min
                                                                                                                                                       // ((((((currentdown(x)@(i+1-maxAttackTime+maxHoldTime)-lastdown)*((i+1)/maxAttackTime)/(currentdown(x)@(i+1-maxAttackTime+maxHoldTime)-lastdown):newshape)*(currentdown(x)@(i+1-maxAttackTime+maxHoldTime)-lastdown)))+lastdown):(_<:((_<lastdown),_):*)),_: min
             )):min(currentdown(x)@maxHoldTime);
    lastdown = 1;

    limiter(N) =
      si.bus(N)<:si.bus(N*2):
      limiter_N_chan(N),
      // ((par(i, N, _*inGain ) : limiter_gain_N_chan(N): par(i, N, attackGain:release:ba.db2linear)));
      ((par(i, N, _*inGain ) : limiter_gain_N_chan(N): par(i, N, (attackRel:release)/30)));

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


    latency = maxHoldTime + (maxAttackTime);
    latency_meter = latency:hbargraph("latency [lv2:reportsLatency] [lv2:integer] [unit:frames]", latency, latency);

    // not really needed:
    // limiter_N_chan(1) = _*inGain <:((limiter_gain_N_chan(1):attackRel:release:meter:ba.db2linear)*_@latency_meter);

    limiter_N_chan(N) =
      (par(i,N,_*inGain )<:
       ((limiter_gain_N_chan(N)),si.bus(N))
      )
      :(ro.interleave(N,2):par(i,N,(attackRel:release:meter:ba.db2linear)*(_@latency_meter)));

    linearXfade(x,a,b) = a*(1-x),b*x : +;

                                       dBgain(x) =
                                         (
                                           ((now >  futuredown) * (min(now,_)))
                                           +
                                           ((now <= futuredown) * now)
                                         )~_ with {
                                   now = currentdown(x)@(maxHoldTime);
                                   futuredown = currentdown(x):slidingMinN(holdTime,maxHoldTime)@max(0,(maxHoldTime - holdTime ));
                                 };

    currentLevel(x)     = ((abs(x)):ba.linear2db);

    currentdownNonRel(x) =
      par(i, maxAttackTime+1, (gain_computer(threshold,knee,currentLevel(x))@i)*(((i+1)/(maxAttackTime+1)) : attackShaper ) ) : minimum(maxAttackTime+1);

    currentdownCalc(x,lastdown) =
      par(i, maxAttackTime+1, (( gain_computer(threshold,knee,currentLevel(x)))@i +lastdown)*(((i+1)/(maxAttackTime+1)) : attackShaper )) : minimum(maxAttackTime+1) -lastdown ;

    currentdown(x) = // currentdownCalc(x)~_;
      gain_computer(threshold,knee,currentLevel(x));

    attackGain(x) =
      // x@maxAttackTime;
      // par(i, maxAttackTime+1, (x@i+x *( ((i+1)/(maxAttackTime+1))) : attackShaper )) : minimum(maxAttackTime+1)-x;

      par(i, maxAttackTime+1, (                x@i) *( ((i+1)/(maxAttackTime+1)) : attackShaper )) : minimum(maxAttackTime+1);

    // par(i, maxAttackTime+1, (x@maxAttackTime+x@i) *( ((i+1)/(maxAttackTime+1)) : attackShaper )) : minimum(maxAttackTime+1)-x@maxAttackTime;

    attackShaper(fraction)= ma.tanh(fraction:pow(attack:attackScale)*(attack*5+.1))/ma.tanh(attack*5+.1);
    attackScale(x) = (x+1):pow(7); //from 0-1 to 1-128, just to make the knob fit the aural experience better
    attack                  = (hslider("[2]attack shape[tooltip: 0 gives a linear attack (slow), 1 a strongly exponential one (fast)]", 0.5 , 0, 1 , 0.001));

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
                      mainGroup(x)        = hgroup("[0]", x);
                      knobGroup(x)     = mainGroup(vgroup("[0]", x));
                      inGain           = knobGroup(hslider("[0]input gain [unit:dB]   [tooltip: input gain in dB ", 0, 0, 30, 0.1)):si.smooth(0.999):ba.db2linear ;
                      threshold        = knobGroup(hslider("[1]threshold [unit:dB]   [tooltip: maximum output level in dB]", 0.0, -60, 0, 0.1):si.smooth(0.999));
                      release_ms       = knobGroup(hslider("[2] release [unit:ms] [scale:log]
[tooltip: Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')]",
                                                           10, 0.1, 500, 0.1)) : *(0.001) : max(1/ma.SR);
                      holdTime         = int(knobGroup(hslider("[3]hold time[unit:ms] [tooltip: hold time in ms][scale:log]", 50, 0.1, maxHoldMs ,0.1))/1000*SampleRate);
                      knee             = knobGroup(hslider("[4] knee [unit:dB] [tooltip: soft knee amount in dB]", 0, 0, 30, 0.1));
                      link             = knobGroup(hslider("[5]link channels[tooltip: 0 means independent, 1 fully linked]", 1, 0, 1 , 0.001));

                      meterGroup(x)    = mainGroup(vgroup("[1]", x));
                      GRmeter_group(x) = meterGroup(hgroup("[0] GR [tooltip: gain reduction in dB]", x));
                      meter            = GRmeter_group(  _<:(_,(_:min(0):max(-20):( (vbargraph("[unit:dB]", -20, 0))))):attach);

                      HUGE_VAL = fconstant(float HUGE_VAL, <math.h>);
