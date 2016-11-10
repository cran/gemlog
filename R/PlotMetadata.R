PlotMetadata = function(M, xlim = range(M$t)){
    t = M$t
    temp = M$temp
    batt = M$batt
    gps = M$gpsOn

    ## in case of very stable power supply, increase the battery plot range
    battrange = range(batt)
    battcent = sqrt(prod(battrange))
    if(battrange[2]/battcent < 1.1){
      battrange = battcent * c(1/1.1, 1.1)
    }

    ylim = min(temp) + (max(temp) - min(temp)) * c(-0.05, 1.15)
    par(mar = c(4,4,4,4))
    plot(t, temp, type = 'l', col = 'red', yaxt = 'n', main = 'Battery and Temperature', xlab = 'Time (day of year)', ylab = 'Battery (V)', ylim = ylim, xlim = xlim)
    lines(t, rescale(batt, ylim, battrange), col='black')
    labels = pretty(battrange)
    axis(2, at = rescale(labels, ylim, battrange), labels = labels, col.axis = 'black', col.ticks = 'black')
    axis(4, at = pretty(range(temp)), col.axis = 'red', col.ticks = 'red')
    mtext('Temperature (deg C)', 4, col = 'red', line = 2.5)
    legend(x = 'topright', pch = c(15,15,15), col = c(1,2,3), legend = c('Battery', 'Temperature', 'GPS'))

    lines(t, gps * 0.02*(ylim[2]-ylim[1]) + ylim[1], col = 3)
}

rescale = function(x, newlim, oldlim = range(x)){
    (x - oldlim[1])/(oldlim[2] - oldlim[1]) * (newlim[2] - newlim[1]) + newlim[1]
}
