import("stdfaust.lib");
import("../LazyLeveler/slidingReduce.lib");

// todo:
// make attack and hold SR dependant
// sample and hold attackShaper shape
// make shape dependent on length

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

process(x) =
  (x: attackRel)
,x@(1*maxAttackTime);
// process =
  // limiter(2);
// (_<:select2(x==x',_+1,_) % size)~_;
// samplesBetweenChange;

clock = ba.time;
wrap(wr,val) = ((val%wr)+wr)%wr;

attackRel(x) =
    attackRelFB~_
  // ,
  // x@maxAttackTime,
  // startGR,
  // endGR
  with {
    attackRelFB(FB) =
      (
      1,// linearXfade( attackCtrl, startGR, endGR ),
      longAttack
      ):min
      // (linearXfade( attackCtrl, startGR, endGR ))
      // (linearXfade( attackCtrlLin, startGRLin, endGR ))
      // ,attackCtrlLin
      // , (x<x')@maxAttackTime
      // , nextLin @maxAttackTime
      // ,((changetime-changetimeLin)/maxAttackTime)
     // ,(prevEnd - prevStart)@maxAttackTime < ( endGRlin - startGRlin )@maxAttackTime
      // < (startGRlin - endGRlin)
        // ,( prevStart - startGRlin )@maxAttackTime
        // ,startGRlin
         // ,endGRlin
          // , par(i,maxAttackTime,array(i)):>_
        // ,prevEnd
      // , nextChange
    , startGRlin
      , endGRlin
      // , linRamp
     // , minTrigger
       // , lowestMin
         // ,attack
     // , lowestMin @maxAttackTime
     // ,attackCtrlMin
     // , (linearXfade( attackCtrl, startGR, endGR ))
     // ,(rampMin/maxAttackTime)@maxAttackTime
     // ,nextMin@maxAttackTime
        // ,nextMin
        // , slidingMinN(maxAttackTime,maxAttackTime,x)@maxAttackTime
        // ,x
        with {

  // linRamp = ((clock - changetime)@maxAttackTime):max(0)/maxAttackTime;
  // linRamp = select2( (x@maxAttackTime)==endGR1, (((clock-changetime)@maxAttackTime)-rampOffset):max(0) ,0 )/maxAttackTime;

  minTrigger = (FB < lowestMin):ba.impulsify;

  longAttack =
    select2(endGRlin>startGRlin,
    linearXfade( linRamp, startGRlin, endGRlin )
  , endGRlin)
    ;


  linRamp = 
  // (Ramp@maxAttackTime)/maxAttackTime;
            (ba.countup(maxAttackTime,x!=x')/maxAttackTime);
    // (ba.countup(maxAttackTime,clock==(changetimeLin))/maxAttackTime);
  // select2( (x@maxAttackTime)==endGR1, (((clock-changetimeLin)@maxAttackTime)-rampOffset):max(0)/maxAttackTime ,0 );

  lowestMin = slidingMinN(maxAttackTime,maxAttackTime,x);

  array(i) =  rwtable(size+2, maxAttackTime, windex , x , i):max(1):min(maxAttackTime); 
  // length =  rwtable(size+2, maxAttackTime, windex , attackCount' , (readIndex+1):wrap(size)@maxAttackTime):max(1):min(maxAttackTime);
  length =  rwtable(size+2, maxAttackTime, windex , attackCount' , (readIndex+1):wrap(size)@maxAttackTime):max(1):min(maxAttackTime);
  lengthMin =  rwtable(size+2, maxAttackTime, windex , attackCountMin' , (readIndex+1):wrap(size)@maxAttackTime):max(1):min(maxAttackTime);

  length1 =  rwtable(size+2, maxAttackTime, windex , attackCount' , (readIndex+1):wrap(size)@maxAttackTime);
  length1Min =  rwtable(size+2, maxAttackTime, windex , attackCountMin' , (readIndex+1):wrap(size)@maxAttackTime);

  changetime =  rwtable(size+2, 0, windex , clock , readIndex);
  changetimeMin =  rwtable(size+2, 0, windex , clock , readIndex);
  changetimeLin = rwtable(size+2, 0, windex, clock , (readIndex-1):wrap(size));

  ramp = select2( (x@maxAttackTime)==endGR1, (((clock-changetime)@maxAttackTime)-rampOffset):max(0) ,0 );
  // rampMin = select2( (x@maxAttackTime)==endGR1Min, (((clock-changetimeMin)@maxAttackTime-rampOffsetMin)):max(0) ,0 );
  rampMin = select2( 0, (clock-changetimeMin):max(0) ,0 );
  // rampMin = select2( 0, (((clock-changetimeMin)@maxAttackTime)):max(0) ,0 );
  attackCtrl = select2( startGR>endGR, 0, (ramp/length) ) :attackShaper;
  attackCtrlLin = select2( startGR>endGR, 0, (ramp/maxAttackTime) );
  attackCtrlMin = select2( startGRMin>endGRMin, 0, (rampMin/lengthMin) ) :attackShaper;
  rampOffset = (length1 - maxAttackTime):max(0);
  rampOffsetMin = (length1Min - maxAttackTime):max(0);


  endGR1   = rwtable(size+2, 0.0, windex, x, readIndex );
  endGR1Min   = rwtable(size+2, 0.0, windex, x, readIndex );

  endGR =     rwtable(size+2, 0.0, windex ,    x , (readIndex+1):wrap(size)@maxAttackTime);
  endGRMin =  rwtable(size+2, 0.0, windex , x , (readIndex):wrap(size)@maxAttackTime);
  endGRlin =
    // rwtable(size+2, 0.0, windex , x , readIndexLin);
  // x:ba.sAndH(x!=x');
    slidingMinN(maxAttackTime,maxAttackTime,x);
  endGRlinNow =
    rwtable(size+2, 0.0, windex , x , (readIndex-0):wrap(size));
  endGRlinPrev =
    rwtable(size+2, 0.0, windex , x , (readIndex-1):wrap(size));
  startGR =    rwtable(size+2, 0.0, windex, x, (readIndex):wrap(size) )@maxAttackTime;
  startGRMin = rwtable(size+2, 0.0, windex, FB, readIndex:wrap(size) );
  startGRlin =
    // rwtable(size+2, 0.0, windex, FB, readIndex);
  FB:ba.sAndH(x!=x');
  startGRlinPrev =
               rwtable(size+2, 0.0, windex, FB, (readIndex-1):wrap(size));

  nextChange = x==x';
  //  ( slidingMinN(maxAttackTime,maxAttackTime,x) == slidingMinN(maxAttackTime,maxAttackTime,x)' )
  //;//          | nextChange;
  windex = select2(nextChange, readIndex, size+1);
  readIndex =    (_<:select2(nextChange,_+1,_) % size)~_;
  readIndexLin =    (readIndex-0):wrap(size);
  // readIndexLin = select2(nextLin, readIndex':ba.sAndH(!nextLin),readIndex);
  // nextLin =   nextChange & smallerNext;
  // nextLin = ((startGRlin-endGRlinNow)<(startGRlinPrev-endGRlinPrev)) * 1;// linRamp<1;
  nextLin = endGRlinNow<endGRlinNow';
     //(endGRlinNow'< (endGRlinNow * (clock-changetimeLin) / maxAttackTime);


  restLength = maxAttackTime - prevLength;
  prevLength = linRamp':ba.sAndH(nextChange*-1+1);

  prevStart = startGRlin':ba.sAndH(nextChange*-1+1); 
  prevEnd = endGRlin':ba.sAndH(nextChange*-1+1);

  
  
  readIndex1 = (readIndex+1):wrap(size);
  readIndex1Min = (readIndex+1):wrap(size);
  attackCount =
  select2(nextChange,0,_+1)~_;
  attackCountMin =
    select2(x==slidingMinN(maxAttackTime,maxAttackTime,x) ,0,_+1)~_;
  size = maxAttackTime;
  attack = ((length / maxAttackTime ):pow(power) - sub ) * mul : max(0) : min(1);
  mul = hslider("mul",2,0.1,9,0.01);
  sub = hslider("sub",0.85,0.1,2,0.01);
  // sub = hslider("sub",0.85,0.1,2,0.01);
  power = hslider("power",0.15,0.01,1,0.01);
  // att = hslider("att",0,0,1,0.1);
  attackShaper(fraction)= ma.tanh(fraction:pow(attack:attackScale)*(attack*5+.1))/ma.tanh(attack*5+.1);
  };
};

// maxHoldTime = 4;
maxHoldTime = pow(2,14);
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
      ((par(i, N, _*inGain ) : limiter_gain_N_chan(N): par(i, N, (release:attackRel)/30)));

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
    // limiter_N_chan(1) = _*inGain <:((limiter_gain_N_chan(1):release:attackRel:meter:ba.db2linear)*_@latency_meter);

    limiter_N_chan(N) =
      (par(i,N,_*inGain )<:
       ((limiter_gain_N_chan(N)),si.bus(N))
      )
      :(ro.interleave(N,2):par(i,N,(release:attackRel:meter:ba.db2linear)*(_@latency_meter)));

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

    attackScale(x) = (x+1):pow(7); //from 0-1 to 1-128, just to make the knob fit the aural experience better
    // attack                  = (hslider("[2]attack shape[tooltip: 0 gives a linear attack (slow), 1 a strongly exponential one (fast)]", 0 , 0, 1 , 0.001));

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

                      testSig =
                        (((((_<:select2(sparse_impulses,_,_-(no.noise:abs/10)))%1)+1)%1)~_)
                      with {
                      freq = abs(no.lfnoise0(100)*50);
                      sparse_impulses =
                        os.lf_imptrain(freq);
                      };
