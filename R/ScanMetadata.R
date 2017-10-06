ScanMetadata = function(fn, plot = TRUE){
    whatnames = scan(fn, what = character(), sep = ',', nlines = 1)
    what = list()
    n = numeric()
    for(i in whatnames)what[[i]] = n
    
    M = scan(fn, what = what, sep = ',', skip=1)
    if(plot){
        PlotMetadata(M)
    }
    invisible(M)
}
