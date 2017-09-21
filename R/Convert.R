Convert = function(rawpath = '.', convertedpath = 'converted', metadatapath = 'metadata', metadatafile = NA, gpspath = 'gps', gpsfile = NA, t1 = -Inf, t2 = Inf, nums = NaN, SN = NaN, bitweight = 0.256/2^15 / (46e-6*3.4/7) / 23.455, time_adjustment = 0, yr = 2016, blockdays = 1){
  ## bitweight: volts/count over transducer sensitivity over gain: given is 0.5" with 2.2k resistor
  ## time adjustment: time (s) to add to time vector as a correction.  015 seems to always need -1.
  
  ## if 'nums' is default, convert all the files in this directory
  if(is.na(nums[1])){
    fn = list.files(rawpath, 'FILE.....TXT')
    nums = as.numeric(substr(fn, 5, 8))
  }
  ## start at the first file in 'nums'
  n1 = min(nums)
  
  ## make sure the raw directory exists and has real data
  if(!dir.exists(rawpath)){
    stop(paste('Raw directory', rawpath, 'does not exist'))
  }
  if(0 == length(list.files(rawpath, 'FILE[[:digit:]]{4}.TXT'))){
    stop(paste('No data files found in directory', rawpath))
  }
  
  ## read the first set of up to (24*blockdays) files
  L = list(t=numeric(), p=numeric(), header=list(), metadata=numeric(), gps=numeric())
  while(length(L$t) == 0){ ## read sets of files until we get one that isn't empty
    if(n1 > max(nums)) return() # if you've made it past the end of nums, just return
    nums_block = nums[nums >= n1 & nums < (n1 + (24*blockdays))]
    L = ReadGem(nums_block, rawpath, alloutput = FALSE, requireGPS = TRUE)
    n1 = n1 + (24*blockdays) # increment file number counter
  }
  t = L$t + time_adjustment#/86400
  p = L$p
                                        #    yr = rep(L$gps$year, length(p))
  
  ## if not specified, define t1 as the earliest integer-second time available
  if(is.infinite(t1)){
    t1 = min(t, na.rm = TRUE)
    t1 = trunc(t1) + 1
  }
  if(is.infinite(t2)){
    t2 = strptime('9999-12-31 23:59:59', '%Y-%m-%d %H:%M:%S', tz = 'GMT') # timekeeping apocalypse
  }
  
  wsn = 0
  while(is.na(SN)){ # take the first non-NA SN. This is important because there can be blank files in there.
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
  
                                        #    # check for matching serial numbers
                                        #    if(FALSE){ # FIX THIS LATER
                                        #    non_empty = which(sapply(L, length) != 0)
                                        #    if(any(L$header$SN[non_empty] != SN)){
                                        #        w = non_empty[L$header$SN != SN]
                                        #        warning(paste('Wrong serial number(s):', paste(L$header$SN[w], collapse=','), ': numbers', paste((nums[nums >= n1 & nums < (n1 + (24*blockdays))])[w], collapse=',')))
                                        #        browser()
                                        #    }
                                        #    }
  
                                        # start metadata and gps files
  metadata = L$metadata
  metadata[,11] = POSIXct2jd(metadata[,11])
  gps = L$gps
  write.table(metadata[metadata$t > POSIXct2jd(t1 - 1),], file = metadatafile, quote=FALSE, sep=',', row.names=FALSE, col.names=TRUE)
  wgps = which(gps$date > POSIXct2jd(t1 - 1))
  if(length(wgps) > 0){
    write.table(gps[wgps,], file=gpsfile, quote=FALSE, sep=',', row.names=FALSE, col.names=TRUE) # formerly in the "else" statement
  }
  
                                        #  browser()
  
                                        # read sets of (24*blockdays) files until all the files are converted
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
    tt2 = min(t2, 0+truncUTC(t1, (86400*blockdays)) + (86400*blockdays) - 0.01, na.rm = TRUE)
    while(max(t, na.rm=TRUE) < tt2 && n1 <= max(nums)){
      L = ReadGem(nums[nums >= n1 & nums < (n1 + (24*blockdays))], rawpath, alloutput = FALSE, requireGPS = TRUE)
      n1 = n1 + (24*blockdays) # increment file counter

      if(length(L$t) == 0) next # skip ahead if there aren't any readable data files here

      ## process newly-read data
      if(any(L$header$SN != SN, na.rm = TRUE) || any(is.na(L$header$SN))){
        w = (L$header$SN != SN) | is.na(L$header$SN)
        warning(paste('Wrong or missing serial number(s):', paste(L$header$SN[w], collapse=','), ': numbers', paste((nums[nums >= n1 & nums < (n1 + (24*blockdays))])[w], collapse=',')))
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
    }
    
    ## run the conversion and write new converted files
    if(any(t >= t1 & t <= tt2)){
      Gem2Segy(list(t=t, p=p, header=list(SN=SN)), dir = convertedpath, t1 = t1, t2 = tt2, bitweight = bitweight, yr = yr)
    }
    
    ## update start time to convert
    t1 = 0+truncUTC(tt2+(86400*blockdays), (86400*blockdays))
  }
}

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
