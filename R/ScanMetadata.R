ScanMetadata = function(fn, plot = TRUE){
    n = numeric()
    what = list(millis=n, batt=n, temp=n, maxWriteTime=n, minFifoFree=n, maxFifoUse=n, maxOverruns=n, gpsOnFlag=n, unusedStack1=n, unusedStackIdle=n, t=n)

    M = scan(fn, what = what, sep = ',', skip=1)
    if(plot){
        PlotMetadata(M)
    }
    invisible(M)
}
