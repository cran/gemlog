Gem2Segy = function(L, dir = '.', yr = 2015, interp = TRUE, starttime = L$t[1]-0.01/86400, t1 = -Inf, t2 = Inf, bitweight = 1/128000 / (46e-6 * 3.4/7) / 23.455){
##### bitweight: (V/count) / (V/Pa) / gain: (4096 mV / 16 / 2^15 count) / (46e-6 V/Pa * 3.3 V/7 V) / 23.455)
    ## adjust bitweight for transducers that are not 0.5"
#    require(RSEIS) # for write1segy...maybe include write1segy and credit JML? #shouldn't be needed anymore--write1segy is part of gem
    eps = 1e-4
    wna = is.na(L$t)
    L$t = L$t[!wna]
    L$p = L$p[!wna]
    P = InterpTime(L, t1 = t1, t2 = t2)

  hours = P$t[0] # empty variable of type POSIX.ct ( c(POSIXct.variable, NULL) doesn't work)
  testhour = as.POSIXct(trunc(min(P$t), units = 'hours')) 
  while(testhour <= max(P$ends)){
    ## for each testhour, check to see whether it falls between a start and end
    if(any(P$starts <= testhour & P$ends >= testhour)){
      ## if it is between a start and end, add it to the hours list
      hours = c(hours, testhour)
    }
    testhour = testhour + 3600 # increment testhour by one hour
  }

  if(any(is.na(c(P$starts, hours)))) browser()
  starts = sort(c(P$starts, hours))
  ends = sort(c(P$ends, hours-eps))
  if(any(is.na(starts))) browser()
    for(i in 1:length(starts)){
        t1 = starts[i]
        t2 = ends[i]
        amp = round(P$p[P$t >= t1 & P$t < t2])
        if(length(amp) == 0) next # just in case this is empty
        t1 = (P$t[P$t >= t1 & P$t < t2])[1]
        sta = paste('0', substr(L$header$SN[1],2,4), sep = '')
        yr = as.numeric(strftime(t1+eps, '%Y', 'GMT'))
        jd = getjul(yr, as.numeric(strftime(t1+eps, '%m', 'GMT')), as.numeric(format(t1+eps, '%d', 'GMT')))
        hr = as.numeric(strftime(t1+eps, '%H', 'GMT'))
        mi = as.numeric(strftime(t1+eps, '%M', 'GMT'))
        sec = as.numeric(strftime(t1+eps, '%OS', 'GMT')) # OS allows fractional seconds
        msec = round(1000*(sec - floor(sec)))
        sec = floor(sec)
#        jd = floor(t1+eps)
#        hr = floor((t1+eps - jd)*24)
#        mi = floor((t1+eps - jd - hr/24)*1440)
#        sec = floor((t1+eps - jd - hr/24 - mi/1440)*86400)
#        msec = round((t1+eps - jd - hr/24 - mi/1440 - sec/86400)*86400000)
        comp = '0'

        ### new year processing
#        if(jd == getjul(yr,12,31)+1){ # wrap-around from ReadGem
#            jd = 1
#            yr = yr + 1
#        }
        
        alist = list(HEAD = list(
                         lineSeq=0,
                         reelSeq=0,
                         event_number=0,
                         channel_number=as.numeric(paste('0x', sta, sep='')), # so that it will work with hex
                         energySourcePt=0,
                         cdpEns=0,
                         traceInEnsemble=0,
                         traceID=1,
                         vertSum=0,
                         horSum=0,
                         dataUse=0,
                         sourceToRecDist=0,
                         recElevation=0,
                         sourceSurfaceElevation=0,
                         sourceDepth=0,
                         datumElevRec=0,
                         datumElevSource=0,
                         sourceWaterDepth=0,
                         recWaterDepth=0,
                         elevationScale=1,
                         coordScale=1,
                         sourceLongOrX=0,
                         sourceLatOrY=0,
                         recLongOrX=0,
                         recLatOrY=0,
                         coordUnits=2,
                         weatheringVelocity=0,
                         subWeatheringVelocity=0,
                         sourceUpholeTime=0,
                         recUpholeTime=0,
                         sourceStaticCor=0,
                         recStaticCor=0,
                         totalStatic=0,
                         lagTimeA=0,
                         lagTimeB=0,
                         delay=0,
                         muteStart=0,
                         muteEnd=0,
                         sampleLength = min(length(amp), 32767),
                         deltaSample = 10000, # microseconds
                         gainType = 1,
                         gainConst = 1,
                         initialGain=0,
                         correlated=0,
                         sweepStart=0,
                         sweepEnd=0,
                         sweepLength=0,
                         sweepType=0,
                         sweepTaperAtStart=0,
                         sweepTaperAtEnd=0,
                         taperType=0,
                         aliasFreq=0,
                         aliasSlope=0,
                         notchFreq=0,
                         notchSlope=0,
                         lowCutFreq=0,
                         hiCutFreq=0,
                         lowCutSlope=0,
                         hiCutSlope=0,
                         year = yr,
                         day = jd,
                         hour = hr,
                         minute = mi,
                         second = sec,
                         timeBasisCode = 2, # UTC
                         traceWeightingFactor =0,
                         phoneRollPos1=0,
                         phoneFirstTrace=0,
                         phoneLastTrace=0,
                         gapSize=0,
                         taperOvertravel=0,
                         station_name = sta,
                         sensor_serial='UNKNOWN',
                         channel_name = sta,
#                         totalStaticHi = 0,
                         totalStaticHi = msec, # wtf with these names
                         samp_rate = 10000, # microseconds
                         data_form = 1,
                         m_secs = msec,
                         trigyear = yr,
                         trigday = jd,
                         trighour = hr,
                         trigminute = mi,
                         trigsecond = sec,
                         trigmills = msec,
                         scale_fac = bitweight,
                         inst_no = as.numeric(paste('0x', sta, sep = '')),
                         not_to_be_used = 0,
                         num_samps = length(amp),
                         max = max(amp),
                         min = min(amp)
            ), amp = amp)
        fn = paste(yr%%100, zeropad(jd, 3), zeropad(hr, 2), zeropad(mi, 2), zeropad(sec, 2), sta, comp, sep = '.')
        if(any(is.na(c(yr, jd, hr, mi, sec)))) browser()
        write1segy(alist, fn = paste(dir, fn, sep = '/'))
        gc()
    }
}
zeropad = function(x, width){
    formatC(x, width = width, flag = '0', digits = 0, format = 'f')
}
