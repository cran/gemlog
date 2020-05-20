ReadGem = function(nums = 0:9999, path = './', SN = character(), units = 'Pa', bitweight = NaN, bitweight_V = NaN, bitweight_Pa = NaN, alloutput = FALSE, verbose = TRUE, requireGPS = FALSE){

  ## determine which format version the file is, and call the appropriate version of ReadGem.
  ## we assume that all files are the same format
  ## format line example: #GemCSV0.85

  ## find the first data file to scan
  fn = list.files(path, '^FILE[[:digit:]]{4}\\.[[:alnum:]]{3}$') # command to find all filenames of the form FILEXXXX.YYY
  num_strings = paste('000', nums, sep = '') 
  num_strings = substr(num_strings, nchar(num_strings) - 3, nchar(num_strings))

  ## num_strings elements are zero-padded numbers 0-9999
  fn = fn[substr(fn, 1, 8) %in% paste('FILE', num_strings, sep = '')]
  fn = paste(path, fn, sep = '/')

  ## try to handle serial number intelligently. if unset by user, first have it default to whatever extension is most common in fn. if still NA after that, default to whatever is saved in the first header.
  ext = substr(fn, nchar(fn)-2, nchar(fn)) # 3-character filename extensions
  if(0 == length(SN) || is.na(SN)){
    for(i in which(ext == 'TXT')){
      ext[i] = ReadSN(fn[i])
        #scan(fn[i], nlines = 1, what = list(character(), character()), sep = ',', skip = 4, quiet = TRUE)[[2]]
    }
    uext = unique(ext)
    SN = uext[which.max(sapply(uext, function(x)sum(ext == x)))]
    if(0 != length(SN) && !is.na(SN)){
      warning(paste('Serial number not set; using', SN))
    }else{
      stop('SN not provided and could not be inferred')
    }
  }

  ## if we know SN, only test files whose extensions are either SN or TXT
  if(0 != length(SN)){ 
    fn = fn[ext %in% c(SN, 'TXT')]
  }

  if(length(fn) == 0){
    warning(paste('No data files found for nums', paste(nums, collapse = ' '), 'in path', path))
    return(NULL)
  }
  ## check to see if any of the files are non-empty
  if(all(file.size(fn) <= 0)){
    warning(paste('No non-empty data files found for nums', paste(nums, collapse = ' '), 'in path', path))
    return(NULL)
  }else if(any(file.size(fn) <= 0)){
    warning('Some files are empty; skipping')
    fn = fn[file.size(fn) > 0]
  }

  ## use the version corresponding to the first file with the correct serial number. So, scan SN from files until you find the right one, then read the format, then break.
  for(i in 1:length(fn)){
    S = ReadSN(fn[i])
    if(length(S) > 0 && as.numeric(SN) == as.numeric(S)){
      S = scan(fn[i], nlines = 1, what = character(), quiet = TRUE)
      version = substr(S, 8, nchar(S))
      break
    }else{
      version = NaN
      warning(paste('No data files found with correct serial number for nums', paste(nums, collapse = ' '), 'in path', path))
      return(NULL)
    }
  }
  
  ## check that same file to see if there's a configuration line. Note that we are assuming that all the files to be read here have the same configuration and same file format version.
  config = ReadConfig(fn[i])
  bitweight_info = GetBitweightInfo(SN = SN, config = config, bitweight = bitweight, bitweight_V = bitweight_V, bitweight_Pa = bitweight_Pa, units = units)
  
  ## ok, we finally know the correct serial number, file format, and bitweight configuration. Read the file with the appropriate function.
  nums = fn2nums(fn)
  if(is.na(version)){
    stop(paste('Unreadable file format version in file', fn[i]))
  }else if(version == '0.9'){
    L = ReadGem_v0.9(nums=nums, path=path, alloutput=alloutput, verbose=verbose, requireGPS=requireGPS, SN=SN)
  }else if(version == '0.85C'){
    L = ReadGem_v0.85C(nums=nums, path=path, alloutput=alloutput, verbose=verbose, requireGPS=requireGPS, SN=SN)
  }else if(version == '0.85'){
    L = ReadGem_v0.85(nums=nums, path=path, alloutput=alloutput, verbose=verbose, requireGPS=requireGPS, SN=SN)
  }else if(version == '0.8'){
    L = ReadGem_v0.8(nums=nums, path=path, alloutput=alloutput, verbose=verbose, requireGPS=requireGPS, SN=SN)
  }else{
    stop(paste('Unrecognized file format in', fn[1]))
  }
    
  n = length(L$header$SN)
  L$header$bitweight = rep(bitweight_info$bitweight, n)
  L$header$bitweight_Pa = rep(bitweight_info$bitweight_Pa, n)
  L$header$bitweight_V = rep(bitweight_info$bitweight_V, n)
  L$header$units = rep(bitweight_info$units, n)
  L$header$gem_version = rep(bitweight_info$version, n)
  L$header$file_format_version = rep(version, n)
  L$header$config = config

  L$p = L$p * bitweight_info$bitweight
  invisible(L)
}

########################################################
ReadSN = function(fn){
  S = scan(fn, nlines = 1, what = list(character(), numeric()), sep = ',', skip = 4, quiet = TRUE)[[2]] # find the SN
  return(S)
}

ReadConfig = function(fn){
  config = list(gps_mode = 1, gps_cycle = 15, gps_quota = 20, adc_range = 0, led_shutoff = 0, serial_output = 0) ## default config: it's fairly safe to use this as the default because any other configuration would require 
  for(j in 1:10){ ## scan the first few lines of the file, looking for a config line
    line = scan(fn, skip = j, nlines = 1, sep = ',', what = character(), quiet = TRUE)
    if(length(line) == 0) next
    if(line[1] == 'C'){
      config = as.list(as.numeric(line[-1]))
      names(config) = c('gps_mode', 'gps_cycle', 'gps_quota', 'adc_range', 'led_shutoff', 'serial_output')
      break
    }
  }
  return(config)
}

fn2nums = function(x){
  as.numeric(substr(sapply(strsplit(x, '/'), function(L)L[[length(L)]]), 5, 9))
}
