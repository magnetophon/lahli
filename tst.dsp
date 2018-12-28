import("stdfaust.lib");

process =
// ((((_ - (no.lfnoise0(freq):abs/10))%1)+1)%1)~_;

  (((((_<:select2(sparse_impulses,_,_-(no.noise:abs/10)))%1)+1)%1)~_)
  // sparse_impulses
// ,
  // select2(sparse_impulses,0,(no.noise:abs/10))
;
  // (no.lfnoise0(freq):abs/10) % 1;

freq = abs(no.lfnoise0(90)):pow(6)*200;

sparse_impulses =
  os.lf_imptrain(freq);
