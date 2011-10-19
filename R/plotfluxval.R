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
# $Id: plotfluxval.R,v 1.8 2011-10-19 12:51:50 steingod Exp $
#

plotfluxval <- function(x,parameter="bias") {

    options(stringsAsFactors=FALSE)

    mytime <- strptime(x$T.sat,"%Y%m%d%H%M")
    myfactor <- factor(paste(x$StId,strftime(mytime,"%Y%m"),sep=" "))
    myylim <- NULL
    
    if (parameter=="bias") {
        myparam <- x$OBS-x$EST
        mytitle <- "Mean bias between observed and estimated"
        myylab <- "W/m²"
        myres <- procdata(myparam,myfactor)
    } else if (parameter=="meanobs") {
        myparam <- x$OBS
        mytitle <- "Mean observed irradiance"
        myylab <- "W/m²"
        myres <- procdata(myparam,myfactor)
    } else if (parameter=="relbias") {
        mytitle <- "Mean error in % of observed irradiance"
        myylab <- "%"
        myres1 <- procdata(x$OBS,myfactor)
        myres2 <- procdata(x$OBS-x$EST,myfactor)
        myres <- list()
        myres$mystations <- myres1$mystations
        tmp <- abs((myres2$data[,2:(length(myres$mystations)+1)]*100)/myres1$data[,2:(length(myres1$mystations)+1)])
        myres$data <- cbind(time=myres1$data[,1],tmp)
        myylim <- c(0,50)
    } else if (parameter=="biassdev") {
        myparam <- x$OBS-x$EST
        mytitle <- "Standard deviation of mean bias"
        myylab <- "W/m²"
        myres <- procdata(myparam,myfactor,action="sdev")
    } else if (parameter=="relbiassdev") {
        mytitle <- "Mean bias standard deviation in % of mean observation"
        myylab <- "%"
        myres1 <- procdata(x$OBS,myfactor)
        myres2 <- procdata(x$OBS-x$EST,myfactor,action="sdev")
        myres <- list()
        myylim <- c(0,50)
        myres$mystations <- myres1$mystations
        tmp <- abs((myres2$data[,2:(length(myres$mystations)+1)]*100)/myres1$data[,2:(length(myres1$mystations)+1)])
        myres$data <- cbind(time=myres1$data[,1],tmp)
    } else if (parameter=="nobs") {
        mytitle <- "Number of cases per station"
        myylab <- "#"
        myres <- procdata(NULL,myfactor,action="nobs")
    } else {
        return("Requested parameter not supported")
    }

    matplot(myres$data[,1],myres$data[,2:(length(myres$mystations)+1)],
    type="p", ylab=myylab,xlab="",xaxt="n",pch=1:length(myres$mystations),
    col=1:length(myres$mystations),ylim=myylim)
    axis.POSIXct(1,myres$data[,1],at=myres$data[,1],labels=strftime(myres$data[,1],"%Y-%m"),las=1,cex.axis=0.8)
    if (parameter=="bias") {
        abline(h=0)
    } else if (parameter=="relbias") {
        abline(h=c(0,10), lty=c(1,2))
    }
    length(myres$mystations)
    xpos <- min(myres$data[,1])
    xpos2 <- max(myres$data[,1]) 
    if (parameter=="relbias") {
        ypos <- 50
    } else {
        ypos <- max(myres$data[,2:(length(myres$mystations)+1)],na.rm=T)
    }
    legend(xpos,ypos,legend=myres$mystations,pch=1:length(myres$mystations),col=1:length(myres$mystations),cex=0.6)
    text(xpos2,ypos, pos=2, labels=mytitle, cex=0.8)
    tmp <- myres$data[,2:(length(myres$mystations)+1)]
    myrowmeans <- rowMeans(tmp,na.rm=T)
    myres$data$rmean <- myrowmeans
    lines(myres$data[,1],myrowmeans,type="b",lwd=3,
    pch=(length(myres$mystations)+1))

    return(myres)
}

procdata <- function(x, myfactor, action="mean") {

    if (action=="sdev") {
        myres <- tapply(x,myfactor,sd,na.rm=T)
    } else if (action=="mean") {
        myres <- tapply(x,myfactor,mean,na.rm=T)
    } else if (action=="nobs") {
        myres <- table(myfactor)
    } else {
        return("Action not supported")
    }

    tmp <- strsplit(as.character(names(myres)),split=" ")
    mykeys <- matrix(unlist(tmp), ncol=2, byrow=T)
    tmp <- cbind(mykeys,myres)

    mymonths <- sort(unique(tmp[,2]))
    mystations <- unique(tmp[,1])
    myres3 <- matrix(nrow=length(mymonths),ncol=length(mystations)+1)
    myres3[,1] <- mymonths
    colnames(myres3) <- c("Time",mystations)
    for (mykey1 in mystations) {
        for (mykey2 in mymonths) {
            if (length(tmp[tmp[,1]==mykey1&tmp[,2]==mykey2,3])>0){ 
                myres3[match(mykey2,myres3),mykey1] <- unclass(tmp[tmp[,1]==mykey1&tmp[,2]==mykey2,3])
            }
        }
    }
    tmp <- myres3
    myres3 <- as.data.frame(tmp)
    myres3[,1] <- as.numeric(strptime(paste(mymonths,"15",sep=""),"%Y%m%d"))
    myres3[,2:(length(mystations)+1)] <- data.matrix(myres3[,2:(length(mystations)+1)])

    return(list(data=myres3,mystations=mystations))
}
