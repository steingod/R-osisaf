#
# PROGRAMMING LANGUAGE:
# S-language (for use in R)
#
# FUNCTION NAME:
# plothist.sl
#
# PURPOSE:
# To read a ASCII file containing collocated data and plot the histogram.
#
# NOTES:
# NA
#
# BUGS:
# NA
#
# AUTHOR:
# Øystein Godøy, DNMI/FOU, 13/06/2001
# MODIFIED:
# Øystein Godøy, DNMI/FOU, 03/11/2001
# Changed basepath...
# Øystein Godøy, met.no/FOU, 16.11.2004
# Adapted for libfmcol data...
#
function(dm=fmcol1,feature="a1") {
    #basepath <- "/raid1/sat/fase/SAF/res_anabase"
    #basepath <- "/disk2/SAGOB/saf"
    #file2open <- paste(basepath,file,sep="/")
    #dm <- read.table(file2open,header=F,skip=5)

    attach(dm)

    par(pty="s")

    if (feature=="a1") {
	hist(k1,col="blue", probability=TRUE)
    } else if (feature=="a2") {
	hist(k2,col="blue", probability=TRUE)
    } else if (feature=="a3") {
	hist(k3,col="blue", probability=TRUE)
    } else if (feature=="t4") { 
	hist(k4,col="blue", probability=TRUE)
    } else if (feature=="t5") { 
	hist(k5,col="blue", probability=TRUE)
    } else if (feature=="d45") { 
	hist(k4-k5,col="blue", probability=TRUE)
    } else if (feature=="r21") { 
	hist(k2/k1,col="blue", probability=TRUE)
    } else if (feature=="r31") { 
	hist(k3/k1,col="blue", probability=TRUE)
    }

    detach(dm)
}
