#
# PROGRAMMING LANGUAGE:
# S-language (for use in R)
#
# FUNCTION NAME:
# mondaily.sl
#
# PURPOSE:
# To monitor SSI estimated from AVHRR data against observed SSI from
# Planteforsks automatic stations. Data are collocated using qc_auto from
# crontab.
# 
# The function gives the option of plotting scatter plots or difference
# plots...
# 
# NOTES:
# NA
#
# BUGS:
# NA
#
# AUTHOR:
# Øystein Godøy, DNMI/FOU, 26/03/2002
# MODIFIED:
# Øystein Godøy, DNMI/FOU, 29/07/2002
# Added RMS and mean observed to difference method...
# Øystein Godøy, met.no/FOU, 04.05.2004
# Modified placement of statistcs in plot...
# Øystein Godøy, METNO/FOU, 22.11.2010
# Modified output.
#
# ID: $Id: mondailyssi.R,v 1.3 2011-02-09 11:18:27 steingod Exp $
#
mondailyssi <- function(filename, method="S",printIt=FALSE) {

    #basedir <- "/disk2/OSI_HL_data/output/flux/ssi/daily/"
    #myfile <- paste(basedir,"qc_list_daily.txt",sep="")
    mydata <- read.table(filename,
	col.names=c("T.sat","EST","NSAT","StId","OBS"),
	na.strings="-999.99"
    )

    if (printIt==TRUE) {
	postscript(paper="special",width=8,height=8,onefile=F,horizontal=F)
    }

    par(pty="s")

    if (method=="S") {
	xdata <- mydata[,"OBS"]
	ydata <- mydata[,"EST"]
	plot(xdata,ydata,
	    xlab="Observed [W/m^2]",ylab="Estimated [W/m^2]")
	abline(0,1)
	mystr1 <- paste("Mean obs.:",
	    formatC(mean(xdata,na.rm=T),format="f",digits=2),
	    "N:",formatC(length(xdata),format="d"))
	mystr2 <- paste("Mean est.:",
	    formatC(mean(ydata,na.rm=T),format="f",digits=2),
	    "N:",formatC(length(ydata),format="d"))
	mystr3 <- paste("Bias:",
	    formatC(mean(xdata-ydata,na.rm=T),format="f",digits=2))
	mystr4 <- paste("RMSD:",
	    formatC(sqrt(mean((xdata-ydata)^2,na.rm=T)),format="f",digits=2))
	mystr5 <- paste("Standard dev.:",
	    formatC(sd(xdata-ydata,na.rm=T),format="f",digits=2))
	mystr<-paste(mystr1,mystr2,mystr3,mystr4,mystr5,sep="\n")
	text(min(xdata,na.rm=T),max(ydata,na.rm=T),adj=c(0,1),mystr)
    } else if (method=="D") {
	mt <- sapply(mydata[,"T.sat"],toString)
	year <- sapply(mt,substr,start=1,stop=4) 
	month <- sapply(mt,substr,start=5,stop=6) 
	day <- sapply(mt,substr,start=7,stop=8)
	hour <- 12
	min <- 00
	xdata <- ISOdatetime(year,month,day,hour,min,0)
	ydata <- mydata[,"OBS"]-mydata[,"EST"]
	plot(xdata,ydata,
	    ylab="Observed-Estimated [W/m^2]")
	abline(0,0)
	mystr1 <- paste("Mean Obs.:",
	    formatC(mean(mydata[,"OBS"],na.rm=T),format="f",digits=2))
	mystr2 <- paste("Mean Est.:",
	    formatC(mean(mydata[,"Est"],na.rm=T),format="f",digits=2))
	mystr3 <- paste("Bias:",
	    formatC(mean(ydata,na.rm=T),format="f",digits=2))
	mystr4 <- paste("RMSD:",
	    formatC(sqrt(mean(ydata^2,na.rm=T)),format="f",digits=2))
	mystr5 <- paste("Standard dev.:",
	    formatC(sd(ydata,na.rm=T),format="f",digits=2))
	mystr6 <- paste("N:",length(ydata))
	mystr<-paste(mystr1,mystr2,mystr3,mystr4,mystr5,mystr6,sep="\n")
	text(xdata[1],max(ydata,na.rm=T),adj=c(0,1),mystr)
    } else if (method=="POS") {
	boxplot((mydata[,"OBS"]-mydata[,"EST"]) ~ mydata[,"StId"],
	    xlab="Station",ylab="Observed-Estimated [W/m^2]",
	    type="p",las=2)
	abline(h=c(-50,-25,0,25,50),lty=2, col=c(2,4,1,4,2))
    }
    title("Daily validation")

    if (printIt==TRUE) {
	dev.off()
    }
}
