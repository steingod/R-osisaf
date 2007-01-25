plotVAChist <- function(vac, classes=c("CLOUD","SEA","ICE"), 
	feature="T4", ploline=FALSE, solarcorrect=TRUE, returnhist=FALSE, ...) {

    t <- getVACclass(vac,classes)

    myhist <- list()

    for (i in 1:length(classes)) {
	u <- getVACclass(t,classes[i])
	if (length(grep("A",feature))>0 && solarcorrect==TRUE) {
	    v <- solzencorrect(u[,"SOZ"],u[,feature])
	} else if (length(grep("R",feature))>0 || length(grep("D",feature))>0) {
	    v <- switch(feature,
		    R21=u[,"A2"]/u[,"A1"],
		    R31=u[,"A3"]/u[,"A1"],
		    D45=u[,"T4"]-u[,"T5"],
		    D34=u[,"T3"]-u[,"T4"],
		    D45=u[,"T3"]-u[,"T4"])
	} else {
	    v <- u[,feature]
	}
	myhist[[i]] <- hist(v,plot=F,...)
	myhist[[i]]$xname <- feature
    }

    xran <- numeric(length=2)
    yran <- numeric(length=2)
    xran[1] <- 1e+05
    xran[2] <- -1e+05
    yran[1] <- 1e+05
    yran[2] <- -1e+05
    for (i in 1:length(classes)) {
	xran[1] <- ifelse(min(myhist[[i]]$breaks,na.rm=T)<xran[1],
		min(myhist[[i]]$breaks,na.rm=T),xran[1])
	xran[2] <- ifelse(max(myhist[[i]]$breaks,na.rm=T)>xran[2],
		max(myhist[[i]]$breaks,na.rm=T),xran[2])
        yran[1] <- ifelse(min(myhist[[i]]$density,na.rm=T)<yran[1],
		min(myhist[[i]]$density,na.rm=T),yran[1])
	yran[2] <- ifelse(max(myhist[[i]]$density,na.rm=T)>yran[2],
		max(myhist[[i]]$density,na.rm=T),yran[2])
    }

    for (i in 1:length(classes)) {
	if (ploline==TRUE) {
	    noofy <- length(myhist[[i]]$breaks)
	    ycoord <- numeric(noofy)
	    ycoord[1] <- 0
	    ycoord[2:noofy] <- myhist[[i]]$density
	}
	if (i==1) {
	    if (ploline==TRUE) {
		plot(myhist[[i]]$breaks,ycoord,
		xlim=xran,ylim=yran,
		main=paste("Histogram of",feature),
		ylab="Density",xlab=feature,
		type="S",
		col=i,...)
	    } else {
		plot(myhist[[i]],xlim=xran,ylim=yran,col=i, freq=F, ...)
	    }
	} else {
	    if (ploline==TRUE) {
		lines(myhist[[i]]$breaks,ycoord,col=i,...)
	    } else {
		lines(myhist[[i]],col=i, freq=F, ...)
	    }
	}
    }
    legend(xran[2],yran[2],classes,xjust=1,fill=1:length(classes),bty="n")

    if (returnhist == TRUE) {
	return(myhist)
    } else {
	return(list(paste("Min X:",xran[1],"Max X:",xran[2],"\n"),
	    paste("Min Y:",yran[1],"Max Y:",yran[2],"\n")))
    }
}


