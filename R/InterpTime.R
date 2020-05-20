InterpTime = function(L, t1 = -Inf, t2 = Inf){
  ## roundoff_starts, roundoff_ends: number of seconds to round off to.  normally 1 or 0.01.  This is important when converting to segy, which starts on integer seconds. #### Deleted on 2016-07-02--JFA (##ok, rounding off to custom units is a major pain with POSIXct.  let's not do that maybe.)
  eps = 0.001 # 1 ms ## 2019-09-11: formerly 1 ms, but this caused the first second after midnight UTC to be skipped...nearly all midnight converted data are therefore of the form ??.???.00.00.01.???.0. Changing it to 0.01 (1 sample) should fix this. Bug dates back to at least 2016-11-10!
    
  ## break up the data vomit into continuous chunks, then round off the starts to the appropriate unit
  ## t1 is the first output sample; should be first integer second after or including the first sample (ceiling)
  t1 = trunc(L$t[which(L$t >= t1)[1]]+1-0.01, units = 'secs') ## 2019-09-11
  t2 = L$t[max(which(L$t <= (t2 + .01 + eps)))] # add a sample because t2 is 1 sample before the hour
  if(t2 <= t1){
    warning(paste0('t1 ', t1, ' >= t2 ', t2, '; skipping'))
    empty_t = Sys.time()[-1]
    return(invisible(list(t = empty_t, p = numeric(), starts = empty_t, ends = empty_t)))
    print('still running') ## should never happen
  }
  breaks_raw = which(diff(L$t) > 0.015)
  breaks = breaks_raw[L$t[breaks_raw] > t1 & L$t[breaks_raw+1] < t2]
  starts = c(t1, L$t[breaks+1]) # start times of continuous chunks
  ends = c(L$t[breaks], t2) # end times of continuous chunks

  w_same = (starts != ends)
  starts = starts[w_same]
  ends = ends[w_same]

  starts_round = trunc(starts, units = 'secs')
  ends_round = trunc(ends+eps+1, units = 'secs')

  ## make an output time vector excluding data gaps, rounded to the nearest samples
  t_interp = Sys.time()[-1] ## make an empty POSIXct 
  for(i in 1:length(starts_round)){
    t_interp = c(t_interp, seq(starts_round[i], ends_round[i] + eps, 0.01)) # eps is to make sure there isn't some stupid rounding problem
  }
  
  t_interp = t_interp[t_interp >= (t1-eps) & t_interp < (t2 + eps)]

  ## interpolate to find pressure at these sample times

  ##p_interp = approx(L$t, L$p, t_interp)$y ## JFA 2019-12-23: linear approximation significantly attenuates higher frequencies. Use cubic instead.
  p_interp = spline(L$t, L$p, xout = t_interp, method = "fmm")$y
  
  invisible(list(t = t_interp, p = p_interp, starts = starts_round, ends = ends_round))
}
