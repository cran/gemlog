InterpTime = function(L, t1 = -Inf, t2 = Inf){
    # roundoff_starts, roundoff_ends: number of seconds to round off to.  normally 1 or 0.01.  This is important when converting to segy, which starts on integer seconds. #### Deleted on 2016-07-02--JFA (##ok, rounding off to custom units is a major pain in the ass with POSIXct.  let's not do that maybe.)
    eps = 0.001 # 1 ms 
    
    # break up the data vomit into continuous chunks, then round off the starts to the appropriate unit

#    t1 = max(min(L$t), t1)
#    t2 = min(max(L$t), t2)
    t1 = L$t[min(which(L$t >= t1))]
    t1 = trunc(t1+1-eps, units = 'secs')
    t2 = L$t[max(which(L$t <= t2))]
    breaks_raw = which(diff(L$t) > 0.015)
    breaks = breaks_raw[L$t[breaks_raw] > t1 & L$t[breaks_raw+1] < t2]
    starts = c(t1, L$t[breaks+1]) # start times of continuous chunks
    ends = c(L$t[breaks], t2) # end times of continuous chunks

    w_same = (starts != ends)
    starts = starts[w_same]
    ends = ends[w_same]

    # formula here: 
#    starts_round = floor(((starts - floor(starts)) * (86400/roundoff_starts) - eps)/(86400/roundoff_starts)
    
#    ends_round = floor(ends) + floor((ends-floor(ends)) * (86400/roundoff_ends) + eps)/(86400/roundoff_ends)
    starts_round = trunc(starts-eps, units = 'secs')
    ends_round = trunc(ends+eps+1, units = 'secs')

    # make an output time vector excluding data gaps, rounded to the nearest samples
    t_interp = Sys.time()[-1]
    for(i in 1:length(starts_round)){
        t_interp = c(t_interp, seq(starts_round[i], ends_round[i] + eps, 0.01)) # eps is to make sure there isn't some stupid rounding problem
    }

    t_interp = t_interp[t_interp >= (t1-eps) & t_interp < (t2 + eps)]

    # interpolate to find pressure at these sample times
    p_interp = approx(L$t, L$p, t_interp)$y

    invisible(list(t = t_interp, p = p_interp, starts = starts_round, ends = ends_round))
}
