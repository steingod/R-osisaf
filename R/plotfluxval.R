#
# NAME:
# plotfluxval
#
# PURPOSE:
# To plot validation results categorized on monthly basis for validation
# stations like CMS are doing...
#
# REQUIREMENTS:
# NA
#
# INPUT:
# NA
#
# OUTPUT:
# NA
#
# NOTES:
# matplot does not accept POSIXct objects as input (or rather, it will be
# treated as any number), thus specification of the x-axis is done in a
# separate step.
#
# BUGS:
# NA
#
# AUTHOR:
# Øystein Godøy, METNO/FOU, 2011-03-18
#
# MODIFIED:
# Øystein Godøy, METNO/FOU, 2011-04-01: Added plotting functionality.
#
# CVS_ID:
# $Id: plotfluxval.R,v 1.4 2011-04-01 20:19:11 steingod Exp $
#

plotfluxval <- function(x,parameter="bias") {

    options(stringsAsFactors=FALSE)

    mytime <- strptime(x$T.sat,"%Y%m%d%H%M")
    myfactor <- factor(paste(x$StId,strftime(mytime,"%Y%m"),sep=" "))
    
    if (parameter=="bias") {
        myparam <- x$OBS-x$EST
        mytitle <- "Mean bias between observed and estimated"
        myylab <- "W/m²"
        myres <- tapply(myparam,myfactor,mean)
    } else if (parameter=="meanobs") {
        myparam <- x$OBS
        mytitle <- "Mean observed irradiance"
        myylab <- "W/m²"
        myres <- tapply(myparam,myfactor,mean)
    } else if (parameter=="meanrelerr") {
        myparam <- (x$EST-x$OBS)*100/x$OBS
        mytitle <- "Mean error in % of observed irradiance"
        myylab <- "%"
    } else {
        return("Requested parameter not supported")
    }

    tmp <- strsplit(as.character(names(myres)),split=" ")
    mykeys <- matrix(unlist(tmp), ncol=2, byrow=T)
    myres2 <- cbind(mykeys,myres)

    mymonths <- sort(unique(myres2[,2]))
    mystations <- unique(myres2[,1])
    myres3 <- matrix(nrow=length(mymonths),ncol=length(mystations)+1)
    myres3[,1] <- mymonths
    colnames(myres3) <- c("Time",mystations)
    for (mykey1 in mystations) {
        for (mykey2 in mymonths) {
            if (length(myres2[myres2[,1]==mykey1&myres2[,2]==mykey2,3])>0){ 
                myres3[match(mykey2,myres3),mykey1] <- unclass(myres2[myres2[,1]==mykey1&myres2[,2]==mykey2,3])
            }
        }
    }
    tmp <- myres3
    myres3 <- as.data.frame(tmp)
    myres3[,1] <- as.numeric(strptime(paste(mymonths,"15",sep=""),"%Y%m%d"))
    myres3[,2:(length(mystations)+1)] <- data.matrix(myres3[,2:(length(mystations)+1)])
    matplot(myres3[,1],myres3[,2:(length(mystations)+1)], type="p",
    ylab=myylab,xlab="",xaxt="n",pch=1:length(mystations))
    axis.POSIXct(1,myres3[,1],at=myres3[,1],labels=strftime(myres3[,1],"%Y-%m"),las=1,cex.axis=0.8)
    #title("Bias Observed-Estimated")
    if (parameter=="bias") {
        abline(h=0)
    }
    length(mystations)
    legend(min(myres3[,1]),max(myres3[,2:(length(mystations)+1)],na.rm=T),legend=mystations,pch=1:length(mystations),col=1:length(mystations),cex=0.8)
    text(max(myres3[,1]),max(myres3[,2:(length(mystations)+1)],na.rm=T),
    pos=2, labels=mytitle, cex=0.8)
    ##legend(min(myres3[,1]),max(myres3[,2:(length(mystations)+1)],na.rm=T),legend=mystations,pch=1:length(mystations),col=1:length(mystations),lty=1:length(mystations),cex=0.8)
    tmp <- myres3[,2:(length(mystations)+1)]
    lines(myres3[,1],rowMeans(tmp,na.rm=T),type="b",lwd=3, pch=(length(mystations)+1))

    return(myres3)
}
