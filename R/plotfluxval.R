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
# NA
#
# CVS_ID:
# $Id: plotfluxval.R,v 1.1 2011-03-22 09:24:16 steingod Exp $
#

plotfluxval <- function(x) {

    mybias <- x$EST-x$OBS
    mytime <- strptime(x$T.sat,"%Y%m%d%H%M")
    myfactor <- factor(paste(x$StId,strftime(mytime,"%Y%m"),sep=" "))
    myres <- tapply(mybias,myfactor,mean)

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
            myres3[match(mykey2,myres3),mykey1] <- myres2[myres2[,1]==mykey1&myres2[,2]==mykey2,3]
            }
        }
    }
    tmp <- myres3
    myres3 <- as.data.frame(tmp)
    myres3[,1] <- as.numeric(strptime(paste(mymonths,"15",sep=""),"%Y%m%d"))
    matplot(myres3[,1],myres3[,2:length(mystations)], type="b",
    ylab="OBS-EST (W/m²)",xlab="",xaxt="n",pch=1:length(mystations))
    axis.POSIXct(1,myres3[,1])
    title("Bias Observed-Estimated")
    abline(h=0)
    legend(locator(),legend=mystations,pch=1:length(mystations),col=1:length(mystations),lty=1:length(mystations))

    return(myres3)
}
