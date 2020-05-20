ReadGem_v0.8= function(nums = 0:9999, path = './', alloutput = FALSE, verbose = TRUE, requireGPS = FALSE, SN = character()){
  preamble_length = 4
  
  ## find the relevant files
  fn = list.files(path, '^FILE[[:digit:]]{4}\\.[[:alnum:]]{3}$') # find all filenames of form FILEDDDD.AAA (D is digits, A is alphanumeric)
  
  num_strings = paste('000', nums, sep = '') 
  num_strings = substr(num_strings, nchar(num_strings) - 3, nchar(num_strings)) # num_strings elements are zero-padded numbers 0-9999
  fn = fn[substr(fn, 1, 8) %in% paste('FILE', num_strings, sep = '')]
  fn = paste(path, fn, sep = '/')
  
  ## try to handle serial number intelligently. if unset by user, first have it default to whatever extension is most common in fn. if still NA after that, default to whatever is saved in the first header.
  ext = substr(fn, nchar(fn)-2, nchar(fn)) # 3-character filename extensions
  if(0 == length(SN) || is.na(SN)){
    uext = unique(ext[ext != 'TXT'])
    SN = uext[which.max(sapply(uext, function(x)sum(ext == x)))]
    if(0 != length(SN) && !is.na(SN)){
      warning(paste('Serial number not set; using', SN))
    }
  }
  
  ## if we know SN, only test files whose extensions are either SN or TXT
  if(0 != length(SN)){ 
    fn = fn[ext %in% c(SN, 'TXT')]
  }
  
  ## set up output list
  empty_time = Sys.time()[-1]
  OUTPUT = list(t = empty_time, p = numeric(), header = list(), metadata = list(), gps = list())
  ## loop through files, processing each individually and appending to OUTPUT
  for(i in 1:length(fn)){
    if(verbose) print(paste('File', i, 'of', length(fn), ':', fn[i]))
    
    L = list()
    
    ## check the serial number. if the default serial number still isn't set, set it on the first file here.
    SN_i = scan(fn[i], skip = preamble_length, nlines = 1, sep = ',', what = character(), quiet = TRUE)[2] # do a separate scan so that in case the SN is wrong, you don't waste time scanning the whole thing
    if((0 == length(SN) || is.na(SN)) && i == 1){
      SN = SN_i
      warning(paste('Serial number not set; using', SN_i))
    }else if(length(SN_i) == 0 || is.na(SN_i) || SN_i != SN){ # skip this file if the serial number is wrong or missing
      warning(paste('Skipping file ', fn[i], ': wrong serial number', sep = ''))
      next
    }
    L$SN = SN_i
    
    ## If we're here, clear to read the whole file
    R = readLines(fn[i])
    
    ## check whether file is empty; skip if necessary
    if(length(R) == 0){
      warning(paste('Skipping empty file', fn[i]))
      next
    }
    
    body = R[(preamble_length+2):length(R)]
    body = body[!is.na(iconv(body))] ## to prevent substr from choking on any multibyte characters (from bad lines)--JFA 2017-10-13
    linetype = substr(iconv(body), 1, 1)
    wg = which(linetype == 'G')
    if(length(wg) == 0 && requireGPS){
      warning(paste('Skipping file (no GPS): ', fn[i]))
      next
    }
    wm = which(linetype == 'M')
    wd = which(linetype == 'D')
    
    ## if we don't have any data, skip the file
    if(length(wd) == 0){
      next
    }
    
    ## Make data matrix
    if(!(requireGPS && length(wg) == 0)){
      L$d = Lines2Matrix(body[wd], 3) # custom function defined below
      if(length(attr(L$d, 'na.values')) > 0){
        warning('Skipping ', length(attr(L$d, 'na.values')), ' corrupt data lines in file ', fn[i], ': could cause timing errors')
        wd = wd[-attr(L$d, 'na.values')]
      }
      L$d = as.data.frame(L$d); names(L$d) = c('millis', 'pressure', 'millisLag')
      ## Make GPS matrix if data are available
      if(length(wg) > 0){ # only do this if there are GPS lines to draw from
        L$g = Lines2Matrix(body[wg], 10)
        if(length(attr(L$g, 'na.values')) > 0){
          warning('Skipping ', length(attr(L$g, 'na.values')), ' corrupt GPS lines in file ', fn[i], ': could cause timing errors')
          wg = wg[-attr(L$g, 'na.values')]
        }
        L$g = as.data.frame(L$g); names(L$g) = c('millis', 'millisLag', 'yr', 'mo', 'day', 'hr', 'min', 'sec', 'lat', 'lon')
        
        L$g$td = getjul(L$g$yr, L$g$mo, L$g$day) + L$g$hr/24 + L$g$min/1440 + L$g$sec/86400
        L$g$tf = 0 + strptime(paste(paste(L$g$yr, L$g$mo, L$g$day, sep = '-'), paste(L$g$hr, L$g$min, L$g$sec, sep=':')), format = '%Y-%m-%d %H:%M:%OS', tz = 'GMT') # %OS allows fractional seconds.  tz='GMT' is their format for UTC. 0 is to force it to be in format POSIXct (which works with lm() ) instead of POSIXlt
      }
    }
    
    ## Make metadata matrix if data are available
    if(length(wm) > 0){ # only do this if there are metadata lines to draw from
      L$m = Lines2Matrix(body[wm], 10)
      if(length(attr(L$m, 'na.values')) > 0){
        warning('Skipping ', length(attr(L$m, 'na.values')), ' corrupt metadata lines in file ', fn[i], ': could cause timing errors')
        wm = wm[-attr(L$m, 'na.values')]
      }
      L$m = as.data.frame(L$m); names(L$m) = c('millis', 'batt', 'temp', 'maxWriteTime', 'minFifoFree', 'maxFifoUsed', 'maxOverruns', 'gpsOnFlag', 'unusedStack1', 'unusedStackIdle')
      L$m$t = rep(NA, length(L$m$millis))
      class(L$m$t) = c('POSIXct', 'POSIXt') # this is necessary to prevent problems with NA coercion to POSIXct, if timing isn't available.
    }
    
    if(!(requireGPS && length(wg)==0)){
      ## Make the millis vector
      millis = 0*1:length(body)
      millis[wd] = L$d$millis
      if(length(wg) > 0){
        millis[wg] = L$g$millis
      }
      if(length(wm) > 0){ 
        millis[wm] = L$m$millis
      }
      
      ## set millis that are not data, metadata, or gps (e.g., raw gps lines for debugging)
      wn = which(!((1:length(millis)) %in% c(wd, wm, wg)))
      if(length(wn) > 0){
        millis[wn[wn>1]] = millis[wn[wn>1]-1] # set these millis equal to the one previous, to avoid any timing errors
        ## if the very first value is not a D, M, or G line, there is no previous value to set millis to. instead, set it to the next one.
        if(any(wn == 1)){
          millis[1] = millis[2]
        }
      }		  
      
      
      ## Fix the millis counts: they're currently saved as 16-bit unsigned integers and need to be unwrapped.  To do this, make a big millis vector so that every millis is in the proper context, unwrap them, then insert them back into the list elements.
      millis_unwrap = unwrap(millis, 2^16)
      
      ## replace data millis
      L$d$millis = millis_unwrap[wd]
      
      ## replace GPS millis
      if(length(wg) > 0){
        L$g$millis = millis_unwrap[wg]
      }
      
      ## replace metadata millis
      if(length(wm) > 0){
        L$m$millis = millis_unwrap[wm]
      }
      
      ## Timekeeping: since we now have frequent GPS strings, relate true time to millis
      if(length(wg) > 1){ # Only do this if GPS strings are available
        
        ## first: detect errors in GPS strings
        n = length(L$g$tf)
        time_min = 0+strptime('1776-07-04 00:00:00', '%Y-%m-%d %H:%M:%S', 'GMT') # happy birthday, america
        time_max = 0+strptime('9999-12-31 23:59:59', '%Y-%m-%d %H:%M:%S', 'GMT') # the real end of the world, for timekeeping
        wna = (L$g$yr == 2000 | L$g$sec != round(L$g$sec) | L$g$lat == 0 | L$g$millisLag > 1000 | L$g$yr > 2025 | L$g$yr < 2014 | L$g$mo > 12 | L$g$day > 31 | L$g$hr > 23 | L$g$min > 59 | L$g$sec > 60 | L$g$tf[1:n] > c(L$g$tf[2:n],time_max) | L$g$tf[1:n] < c(time_min, L$g$tf[1:(n-1)]) ) # lines where the year is 2000 generally have bad year/month/date and bad lat/lon.  time might be ok but might as well just toss it for now to be safe.  I suspect timing errors are high for L$g$sec with non-integer values. ## eliminate L$g$td[1:n] > c(L$g$td[2:n],Inf) | L$g$td[1:n] < c(-Inf, L$g$td[1:(n-1)])  because of new year possibility
        wna = wna & !is.na(wna)
        wgood = !wna
        
### outliers can be a problem in the next step, so weed them out here.  Outlying residuals are 3*sigma.  Toss them and recalculate.  Do it here in case we lose all the data somehow (probably not possible, but be safe)!
        if(sum(wgood) > 1){
          l = lm(L$g$tf[wgood] ~ L$g$millis[wgood])
          l$residuals = as.numeric(l$residuals)
        }
        while(any(abs(l$residuals) > 3*sd(l$residuals)) && sum(wgood) > 1){
          wgood[which(wgood)[abs(l$residuals) > 3*sd(l$residuals)]] = FALSE
          wna = !wgood
          l = lm(L$g$tf[wgood] ~ L$g$millis[wgood])
          l$residuals = as.numeric(l$residuals)
        }
        
        ## replace bad GPS data with NaN
        if(sum(wna)>0){
          L$g[wna,3:11] = NaN
          wg = wg[wgood]
          L$g = L$g[wgood,]
        }
      }
      
      ## calculate clock drift with best fit line, using corrected L$g (bad values removed)
      if(length(wg)>1){
        l = lm(L$g$tf ~ L$g$millis)
        l$residuals = as.numeric(l$residuals)
        timecorrected = c(empty_time, l$coefficients[1] + millis_unwrap * l$coefficients[2]) # empty time is to make it the POSIXct class
        L$g$t = timecorrected[wg]
        L$d$t = timecorrected[wd]
        if(length(wm) > 0){
          L$m$t = timecorrected[wm]
        }
      }else{ ## No GPS data available, so set "true times" as NaN
        if(length(wm) > 0){
          L$m$t = L$m$millis + NA # needs to be NA to prevent problems when coercing to POSIXct
          class(L$m$t) = c('POSIXct', 'POSIXt')
        }
        L$d$t = L$d$millis + NA
      }
      
      ## check for wrong sample intervals (0.0025 s errors)
      sampledelayerrors = abs((diff(L$d$t) - 0.01))
      if(any(sampledelayerrors > 0.0025, na.rm = TRUE)){
        warning('File ', fn[i], ': ', sum(sampledelayerrors > 0.0025), ' samples with time errors > 0.0025 s; max time error ', max(sampledelayerrors))
      }
    }
    ## append all the data from the current file to the output list
    OUTPUT$t = c(OUTPUT$t, L$d$t)
    OUTPUT$p = c(OUTPUT$p, L$d$pressure)
    
    ## append metadata if present
    if(length(wm) > 0){
      OUTPUT$metadata = rbind(OUTPUT$metadata, L$m)
    }
    
    ## append GPS data if present
    if(length(wg) > 1){
      OUTPUT$gps = rbind(OUTPUT$gps, as.data.frame(cbind(year = L$g$yr, date = L$g$td, lat = L$g$lat, lon = L$g$lon)))#[!wna,])# 2020-01-01: wna is redundant (has already been used to eliminate NaNs above, as wgood)
    }
    
    ## append header info: each item is a vector with each element corresponding to a file
    OUTPUT$header$file[i] = fn[i] # file name
    OUTPUT$header$SN[i] = L$SN # serial number
    if(length(wg) != 0){ # add location/time data to header if GPS data available
      OUTPUT$header$lat[i] = mean(L$g$lat, na.rm=TRUE) 
      OUTPUT$header$lon[i] = mean(L$g$lon, na.rm = TRUE)
      OUTPUT$header$t1[i] = min(L$d$t) # start time
      OUTPUT$header$t2[i] = max(L$d$t) # end time
    }else{ # if no gps data, don't attempt to include time/location in header
      OUTPUT$header$lat[i] = NaN
      OUTPUT$header$lon[i] = NaN
      OUTPUT$header$t1[i] = NaN
      OUTPUT$header$t2[i] = NaN
    }            
    
    ## return all output if wanted
    if(alloutput){
      OUTPUT$header$alloutput[[i]] = L
    }
    
    ## warn the user if there's no GPS data (common if recorded inside)
    if(!requireGPS && length(wg) == 0){
      warning('File ', fn[i], ': No timing/location info available from GPS')
    }
    
    ## warn the user if there's no metadata (rare--should only happen for very short files < 10 s)
    if(length(wm) == 0){
      warning('File ', fn[i], ': No metadata available')
    }
    print(c(length(L$d$t), length(L$d$pressure)))
    ## done with current file; move on to the next
  }
  ## return the output
  invisible(OUTPUT)
}





Lines2Matrix = function(x, n){
    ## function to read a series of lines as strings and parse them
    ## ensures that line separations are preserved in case a line is corrupted
    ## if any parts of a line are NaN, the whole thing is suspect so make it all NaN
    NaNcheck = function(x){ # NaNcheck scans a vector of numbers, checks for NaNs, and if any NaNs exist, make everything NaN.  otherwise, return the line.
        q = as.numeric(x[1 + 1:n])
        if(any(is.na(q))) rep(NaN, n) else q
    }
    y = t(sapply(strsplit(x, ','), NaNcheck)) # split the string by commas and check for NaNs
    w = is.na(y[,1])
    y = y[!w,]
    if(length(y) == n) y = matrix(y, 1, n) # necessary when only 1 metadata line exists
    attr(y, 'na.values') = which(w)
    y
}

unwrap = function(x, m){
    cumsum(c(x[1], (((diff(x)+m/2) %% m)-m/2)))
}
        
