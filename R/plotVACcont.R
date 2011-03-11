#
# Thsi is probably not working yet!!!!
#
# ID: $Id: plotVACcont.R,v 1.1 2011-03-11 08:45:12 steingod Exp $
#

plotVACcont <- function(vac,classes=c("CLOUD","SEA","ICE"),
	features=c("R21","R31"),solarcorrect=TRUE,
	...) {

    if (length(features)!=2) {
	return("Two features required...\n")
    }

    t <- getVACclass(vac,classes)

    x <- list()
    myhist <- list()

    for (i in 1:length(classes)) {
	u <- getVACclass(t,classes[i])
	for (j in 1:length(features)) {
	    if (length(grep("A",features[j]))>0 && solarcorrect==TRUE) {
		v <- solzencorrect(u[,"SOZ"],u[,features[j]])
	    } else if (length(grep("R",features[j]))>0 || 
		    length(grep("D",features[j]))>0) {
		v <- switch(feature,
			R21=u[,"A2"]/u[,"A1"],
			R31=u[,"A3"]/u[,"A1"],
			D45=u[,"T4"]-u[,"T5"],
			D34=u[,"T3"]-u[,"T4"])
	    } else {
		v <- u[,feature]
	    }
	    myhist[[i]] <- c(hist(v,plot=F,...))
	    myhist[[i]]$xname <- feature
	}
    }
    
}
