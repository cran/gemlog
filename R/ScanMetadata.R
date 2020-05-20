ScanMetadata = function(fn, plot = TRUE){
    whatnames = scan(fn, what = character(), sep = ',', nlines = 1)
    what = list()
    n = numeric()
    for(i in whatnames)what[[i]] = n
    ti = which(whatnames == 't')
    what[[ti]] = character()
    
    M = scan(fn, what = what, sep = ',', skip=1, flush = TRUE)

    tnum = as.numeric(M[[ti]])
    if(is.na(tnum[1])){
      M[[ti]] = as.POSIXct(M[[ti]], origin = '1970-01-01')
    }else{
      M[[ti]] = tnum
    }
    
    if(plot){
        PlotMetadata(M)
    }
    invisible(data.frame(M))
}
