Convert = function(rawpath = '.', convertedpath = 'converted', metadatapath = 'metadata', metadatafile = NA, gpspath = 'gps', gpsfile = NA, t1 = -Inf, t2 = Inf, nums = NaN, SN = character(), bitweight = NaN, units = 'Pa', time_adjustment = 0, blockdays = 1){
  ## bitweight: leave blank to use default (considering Gem version, config, and units). This is preferred when using a standard Gem (R_g = 470 ohms)
  
  ## if 'nums' is default, convert all the files in this directory
  if(is.na(nums[1])){
    if(length(SN) == 1){
      fn = list.files(rawpath, paste0('FILE[[:digit:]]{4}.(TXT|', SN, ')'))
    }else{
      fn = list.files(rawpath, 'FILE[[:digit:]]{4}....')
    }
    nums = as.numeric(substr(fn, 5, 8))
  }
  ## start at the first file in 'nums'
  n1 = min(nums)
  
  ## make sure the raw directory exists and has real data
  if(!dir.exists(rawpath)){
    stop(paste('Raw directory', rawpath, 'does not exist'))
  }
  if(0 == length(list.files(rawpath, 'FILE[[:digit:]]{4}....'))){
    stop(paste('No data files found in directory', rawpath))
  }

  ## make sure bitweight is a scalar
  if(length(bitweight) != 1){
    stop('bitweight must have length 1 (cannot be a vector, or empty)')
  }
  
  ## read the first set of up to (12*blockdays) files
  L = list(t=numeric(), p=numeric(), header=list(), metadata=numeric(), gps=numeric())
  while(length(L$t) == 0){ ## read sets of files until we get one that isn't empty
    if(n1 > max(nums)) return() # if you've made it past the end of nums, just return
    nums_block = nums[nums >= n1 & nums < (n1 + (12*blockdays))] # files are 2 hours, so 12 files = 24 hours
    L = ReadGem(nums_block, rawpath, alloutput = FALSE, requireGPS = TRUE, SN = SN, units = 'counts') # request data in counts so it can be written to segy as ints. conversion to physical units is provided as a header element in the file.
    
    n1 = n1 + (12*blockdays) # increment file number counter
  }
  t = L$t + time_adjustment#/86400
  p = L$p
  yr = rep(L$gps$year[1], length(t))
    
  ## if bitweight isn't set, use the default bitweight for the logger version, config, and units
  if(is.na(bitweight)){
    if(units == 'Pa'){
      bitweight = L$header$bitweight_Pa[1]
    }else if(units == 'V'){
      bitweight = L$header$bitweight_V[1]
    }else if (units == 'Counts' || units == 'counts'){
      bitweight = 1
    }
  }
      
  ## if not specified, define t1 as the earliest integer-second time available
  if(is.infinite(t1)){
    t1 = min(t, na.rm = TRUE)
    t1 = trunc(t1) + 1
  }
  if(is.infinite(t2)){
    t2 = strptime('9999-12-31 23:59:59', '%Y-%m-%d %H:%M:%S', tz = 'GMT') # timekeeping apocalypse
  }
  
  wsn = 0
  while(length(SN) == 0 || is.na(SN)){ # take the first non-NA SN. This is important because there can be blank files in there.
    wsn = wsn+1
    SN = L$header$SN[wsn]
  }
  
  ## set up the gps and metadata files. create directories if necessary
  if(is.na(gpsfile)){
    if(!dir.exists(gpspath)){
      dir.create(gpspath, recursive = TRUE) # recursive means if gpspath = dir1/dir2, and dir1 doesn't exist, that dir1 will be created and then dir1/dir2 will be created
    }
    gpsfile = makefilename(gpspath, SN, 'gps')
    
  }
  
  if(is.na(metadatafile)){
    if(!dir.exists(metadatapath)){
      dir.create(metadatapath, recursive = TRUE) 
    }
    metadatafile = makefilename(metadatapath, SN, 'metadata')
  }
  
  ## if the converted directory does not exist, make it
  if(!dir.exists(convertedpath)){
    dir.create(convertedpath, recursive = TRUE)
  }
  
  ## start metadata and gps files
  metadata = L$metadata
  metadata$t = POSIXct2jd(metadata$t) # depending on format, t might not be column 11
  gps = L$gps
  write.table(metadata[metadata$t > POSIXct2jd(t1 - 1),], file = metadatafile, quote=FALSE, sep=',', row.names=FALSE, col.names=TRUE)
  wgps = which(gps$date > POSIXct2jd(t1 - 1))
  if(length(wgps) > 0){
    write.table(gps[wgps,], file=gpsfile, quote=FALSE, sep=',', row.names=FALSE, col.names=TRUE) # formerly in the "else" statement
  }
  
  ## read sets of (12*blockdays) files until all the files are converted
  while(TRUE){
    ## toss old samples
    w = which(t > (t1-0.01))
    t = t[w]
    p = p[w]
    yr = yr[w]
    
    ## check to see if we're done
    if(n1 > max(nums) & length(p) == 0) break # out of raw data to convert
    if((t1 > t2) && !is.na(t1 > t2)) break # already converted the requested data
    
    print(strftime(t1, tz = 'GMT'))
    
    ## load new data if necessary
    tt2 = min(t2, 0+truncUTC(t1, (86400*blockdays/2)) + (86400*blockdays/2) - 0.01, na.rm = TRUE)
    while(max(t, na.rm=TRUE) < tt2 && n1 <= max(nums)){
      L = ReadGem(nums[nums >= n1 & nums < (n1 + (12*blockdays))], rawpath, alloutput = FALSE, requireGPS = TRUE, SN = SN, units = 'counts')
      n1 = n1 + (12*blockdays) # increment file counter

      if(length(L$t) == 0) next # skip ahead if there aren't any readable data files here

      ## process newly-read data
      if(any(L$header$SN != SN, na.rm = TRUE) || any(is.na(L$header$SN))){
        w = (L$header$SN != SN) | is.na(L$header$SN)
        warning(paste('Wrong or missing serial number(s):', paste(L$header$SN[w], collapse=','), ': numbers', paste((nums[nums >= n1 & nums < (n1 + (12*blockdays))])[w], collapse=',')))
        ## browser()
      }
      
      ## append new data to old
      t = c(t, L$t)
      p = c(p, L$p)
      yr = c(yr, rep(L$gps$year[1], length(L$t)))
      
      ## update the metadata file
      L$metadata[,11] = POSIXct2jd(L$metadata[,11])
      write.table(L$metadata, metadatafile, quote = FALSE, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
      
      ## update the gps file
      write.table(L$gps, gpsfile, quote = FALSE, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
    } # while max(t)<tt2 && n1 <= max(nums)

    ## run the conversion and write new converted files
    if(any(t >= t1 & t <= tt2)){
      Gem2Segy(list(t=t, p=p, header=list(SN=SN)), dir = convertedpath, t1 = t1, t2 = tt2, bitweight = bitweight, yr = yr)
    }
    
    ## update start time to convert
    t1 = 0+truncUTC(tt2+(86400*blockdays/2), (86400*blockdays/2))
    if(tt2 < min(t) && t2 > min(t)){
      t1 = min(t) - 1
    }
  } ## while TRUE
}
##########################################
truncUTC = function(x, n=86400)as.POSIXct((as.numeric(x) %/% n)*n, origin='1970-01-01')

makefilename = function(dir, SN, type){
  n = 0
  fn = paste(dir, '/', SN, type, '_', formatC(n, width = 3, flag = 0), '.txt', sep = '')
  while(file.exists(fn)){
    n = n + 1
    fn = paste(dir, '/', SN, type, '_', formatC(n, width = 3, flag = 0), '.txt', sep = '')
  }
  return(fn)
}

POSIXct2jd = function(x){
  output = rep(0, length(x))
  for(i in 1:length(x)){
    if(is.na(x[i])){
      output[i] = NaN
    }else if(is.numeric(x[i])){
      tmp = x[i] + as.POSIXct('1970-01-01', format = '%Y-%m-%d', tz = 'GMT')
      output[i] = unclass(julian(tmp, origin = as.POSIXct(paste(format(tmp, '%Y'),'-01-01', sep = ''), tz="GMT")))[1] + 1
    }else{
      output[i] = unclass(julian(x[i], origin = as.POSIXlt(paste(format(x[i], '%Y'),'-01-01', sep = ''), tz="GMT")))[1] + 1 ## 2016-09-24: rearranged parentheses to include tz as an argument to as.POSIXlt, not julian.
    }
  }
  return(output)
}
