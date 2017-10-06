ReadGem = function(nums = 0:9999, path = './', alloutput = FALSE, verbose = TRUE, requireGPS = FALSE, SN = character()){

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
      ext[i] = scan(fn[i], nlines = 1, what = list(character(), character()), sep = ',', skip = 4, quiet = TRUE)[[2]]
    }
    uext = unique(ext)
    SN = uext[which.max(sapply(uext, function(x)sum(ext == x)))]
    if(0 != length(SN) && !is.na(SN)){
      warning(paste('Serial number not set; using', SN))
    }
  }

  ## if we know SN, only test files whose extensions are either SN or TXT
  if(0 != length(SN)){ 
    fn = fn[ext %in% c(SN, 'TXT')]
  }

  if(length(fn) == 0){
    stop('No data files found for nums and path')
    return(NULL)
  }

  ## use the version corresponding to the first file with the correct serial number. So, scan SN from files until you find the right one, then read the format, then break.
  for(i in 1:length(fn)){
    S = scan(fn[i], nlines = 1, what = list(character(), numeric()), sep = ',', skip = 4, quiet = TRUE)[[2]] # find the SN
    if(as.numeric(SN) == as.numeric(S)){
      S = scan(fn[i], nlines = 1, what = character(), quiet = TRUE)
      version = substr(S, 8, nchar(S))
      break
    }else{
      version = NaN
    }
  }

  ## ok, we finally know the correct serial number and file format. Read the file with the appropriate function.
  if(is.na(version)){
    stop('No data files had the correct serial number')
  }else if(version == '0.85'){
    invisible(ReadGem_v0.85(nums=nums, path=path, alloutput=alloutput, verbose=verbose, requireGPS=requireGPS, SN=SN))
  }else if(version == '0.8'){
    invisible(ReadGem_v0.8(nums=nums, path=path, alloutput=alloutput, verbose=verbose, requireGPS=requireGPS, SN=SN))
  }else{
    stop(paste('Unrecognized file format in', fn[1]))
  }
}
