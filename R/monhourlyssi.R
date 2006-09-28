#
# PROGRAMMING LANGUAGE:
# S-language (for use in R)
#
# FUNCTION NAME:
# monssi.sl
#
# PURPOSE:
# To monitor SSI estimated from AVHRR data against observed SSI from
# Planteforsks automatic stations. Data are collocated using qc_auto from
# crontab.
# 
# The function gives the option of plotting scatter plots or difference
# plots by index or by hour. Not all text is implemented yet...
# 
# NOTES:
# This function should be changed, now too much datahandling is performed
# while plotting, this should be performed while selecting data. Sometime
# in the future this should be changed, for now it works...
#
# NOTES:
# Possible plotting methods are:
# S: scatter plot - all data
# SC: scatter plot - clear data (by minutes of direct insolation)
# SO: scatter plot - overcast data (by minutes of direct insolation)
# D: difference plot
# DC: difference plot by cloud cover (PPS estimated)
# DTS: difference plot as time series - all data
# DTSC: difference plot as time series - clear data (by insolation)
# DTSO: difference plot as time series - overcast data (by insolation)
# DTSCC: difference plot as time series - clear data (by insolation and
# PPS)
# DTSOO: difference plot as time series - overcast data (by insolation and
# PPS)
# DSAT: difference plot by satellite id - all data
# DSATC: difference plot by satellite id - clear data (by insolation)
# DSATCC: difference plot by satellite id - clear data (by insolation and
# PPS)
# DSATO: difference plot by satellite id - overcast data (by insolation)
# DSATOO: difference plot by satellite id - overcast data (by insolation
# and PPS)
# DSOZ: difference plot by solar zenith angle
# DSAZ: difference plot by satellite zenith angle
# DRAZ: difference plot by relative azimuth angle
# DH: difference plot by satellite time (hour of day)
# STCC: minutes of direct insolation versus cloud cover (validates PPS)
#
# BUGS:
# NA
#
# AUTHOR:
# �ystein God�y, DNMI/FOU, 26/03/2002
# MODIFIED:
# �ystein God�y, DNMI/FOU, 29/04/2002
# Added some basic functionality concerning cloud no cloud...
# �ystein God�y, met.no/FOU, 04.05.2004
# Modified placement of statistics boxes in plots...
# �ystein God�y, met.no/FOU, 13.05.2004
# Added DTSOO, and changed how cloud information is handled (now between 1
# and 2).
#
monhourlyssi <- function(basedir,method="S",area="a",printIt=FALSE,thc=1.1,tho=1.9,sat="a") {

    #thc <- 2
    #tho <- 4

    # Read data
    #basedir <- "/disk2/OSI_HL_data/output/flux/ssi/product/"
    if (area=="ns" || area=="nr") {
	myfile <- paste(basedir,"ssiqc_list_",area,".txt",sep="")
	mydata <- read.table(myfile,
	    col.names=
	    c("T.sat","T.obs","StId","TTM","OBS","ST","EST","NVAL","N",
	    "SAT","SOZ","SAZ","RAZ","CM"),
	    na.strings="-999.00")
    } else if (area=="a") {
	myfile <- paste(basedir,"ssiqc_list_ns.txt",sep="")
	mydata1 <- read.table(myfile,
	    col.names=
	    c("T.sat","T.obs","StId","TTM","OBS","ST","EST","NVAL","N",
	    "SAT","SOZ","SAZ","RAZ","CM"),
	    na.strings="-999.00")
	myfile <- paste(basedir,"ssiqc_list_nr.txt",sep="")
	mydata2 <- read.table(myfile,
	    col.names=
	    c("T.sat","T.obs","StId","TTM","OBS","ST","EST","NVAL","N",
	    "SAT","SOZ","SAZ","RAZ","CM"),
	    na.strings="-999.00")
	mydata <- rbind(mydata1, mydata2)
    } else {
	return("Area was not specified!")
    }

    if (printIt==TRUE) {
	postscript(paper="special",width=8,height=8,onefile=F,horizontal=F)
    }

    par(pty="s")

    # Plot data
    if (method=="S") {
	plot(mydata[,"OBS"],mydata[,"EST"],
	    xlab="Observed [W/m^2]",ylab="Estimated [W/m^2]")
	abline(0,1)
	mystr1 <- paste("Mean obs.:",
	    formatC(mean(mydata[,"OBS"],na.rm=T),format="f",digits=2),
	    "N:",formatC(length(mydata[,"OBS"]),format="d"))
	mystr2 <- paste("Mean dif.:",
	    formatC(mean(mydata[,"OBS"]-mydata[,"EST"],na.rm=T),
	    format="f",digits=2),
	    "SD:",
	    formatC(sd(mydata[,"OBS"]-mydata[,"EST"],na.rm=T),
	    format="f"))
	mystr3 <- paste(
	    "RMSD:",
	    formatC(sqrt(mean(((mydata[,"OBS"]-mydata[,"EST"])^2),na.rm=T)),
		format="f",digits=2))
	mystr<-paste(mystr1,mystr2,mystr3,sep="\n")
	text(0.4,max(mydata[,"EST"],na.rm=T),
	    adj=c(0,1),mystr)
	title("All data")
    } else if (method=="SC") {
	plot(mydata[mydata[,"ST"]==60,"OBS"],mydata[mydata[,"ST"]==60,"EST"],
	    xlab="Observed [W/m^2]",ylab="Estimated [W/m^2]")
	abline(0,1)
	mystr1 <- paste("Mean obs.:",
	    formatC(mean(mydata[mydata[,"ST"]==60,"OBS"],na.rm=T),
		format="f",digits=2),
	    "N:",formatC(length(mydata[mydata[,"ST"]==60,"OBS"]),format="d"))
	mystr2 <- paste("Mean dif.:",
	    formatC(mean(mydata[mydata[,"ST"]==60,"OBS"]-
		mydata[mydata[,"ST"]==60,"EST"],na.rm=T),
	    format="f",digits=2),
	    "SD:",
	    formatC(sd(mydata[mydata[,"ST"]==60,"OBS"]-
		mydata[mydata[,"ST"]==60,"EST"],na.rm=T),
	    format="f"))
	mystr3 <- paste(
	    "RMSD:",
	    formatC(sqrt(mean(((mydata[mydata[,"ST"]==60,"OBS"]-
		mydata[mydata[,"ST"]==60,"EST"])^2),na.rm=T)),
		format="f",digits=2))
	mystr<-paste(mystr1,mystr2,mystr3,sep="\n")
	text(0,
	    max(mydata[mydata[,"ST"]==60,"EST"],na.rm=T),adj=c(0,1),mystr)
	title("Clear [ST=60]")
    } else if (method=="SO") {
	plot(mydata[mydata[,"ST"]==0,"OBS"],mydata[mydata[,"ST"]==0,"EST"],
	    xlab="Observed [W/m^2]",ylab="Estimated [W/m^2]")
	abline(0,1)
	mystr1 <- paste("Mean obs.:",
	    formatC(mean(mydata[mydata[,"ST"]==0,"OBS"],na.rm=T),
		format="f",digits=2),
	    "N:",formatC(length(mydata[mydata[,"ST"]==0,"OBS"]),format="d"))
	mystr2 <- paste("Mean dif.:",
	    formatC(mean(mydata[mydata[,"ST"]==0,"OBS"]-
		mydata[mydata[,"ST"]==0,"EST"],na.rm=T) ,
	    format="f",digits=2),
	    "SD:",
	    formatC(sd(mydata[mydata[,"ST"]==0,"OBS"]-
		mydata[mydata[,"ST"]==0,"EST"],na.rm=T),
	    format="f")
	    )
	mystr3 <- paste(
	    "RMSD:",
	    formatC(sqrt(mean(((mydata[mydata[,"ST"]==0,"OBS"]-
		mydata[mydata[,"ST"]==0,"EST"])^2),na.rm=T)),
		format="f",digits=2))
	mystr<-paste(mystr1,mystr2,mystr3,sep="\n")
	text(0,
	    max(mydata[mydata[,"ST"]==0,"EST"],na.rm=T),adj=c(0,1),mystr)
	title("Overcast [ST=0]")
    } else if (method=="D") {
	plot(mydata[,"OBS"]-mydata[,"EST"],
	    ylab="Observed-Estimated [W/m^2]")
	abline(0,0)
	mystr1 <- paste("Mean dif.:",
	    mean(mydata[,"OBS"]-mydata[,"EST"],na.rm=T))
	mystr2 <- paste("Standard dev.:",
	    sd(mydata[,"OBS"]-mydata[,"EST"],na.rm=T))
	mystr3 <- paste("N:",length(mydata[,"OBS"]))
	mystr<-paste(mystr1,mystr2,mystr3,sep="\n")
	text(10,max(mydata[,"OBS"]-mydata[,"EST"],na.rm=T),adj=c(0,1),mystr)
    } else if (method=="DSOZ") {
	plot(mydata[,"SOZ"],mydata[,"OBS"]-mydata[,"EST"],
	    xlab="Solar zenith angle",ylab="Observed-Estimated [W/m^2]",
	    xlim=c(0,100))
	abline(0,0)
    } else if (method=="DSAZ") {
	plot(mydata[,"SAZ"],mydata[,"OBS"]-mydata[,"EST"],
	    xlab="Satellite zenith angle",ylab="Observed-Estimated [W/m^2]",
	    xlim=c(0,80))
	abline(0,0)
    } else if (method=="DRAZ") {
	plot(mydata[,"RAZ"],mydata[,"OBS"]-mydata[,"EST"],
	    xlab="Relative azimuth angle",ylab="Observed-Estimated [W/m^2]",
	    xlim=c(0,360))
	abline(0,0)
    } else if (method=="DSAT") {
	plot(mydata[,"SAT"],mydata[,"OBS"]-mydata[,"EST"],
	    xlab="Satellite",ylab="Observed-Estimated [W/m^2]",type="p")
	abline(0,0)
	mystr16 <- paste("NOAA-16 | Mean:",
	    formatC(mean(
		mydata[mydata[,"SAT"]=="NOAA-16","OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-16","EST"],na.rm=T),
		format="f",digits=2),
	    "Median:",
	    formatC(median(
		mydata[mydata[,"SAT"]=="NOAA-16","OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-16","EST"],na.rm=T),
		format="f",digits=2),
	    "SD:",
	    formatC(sd(
		mydata[mydata[,"SAT"]=="NOAA-16","OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-16","EST"],na.rm=T),
		format="f",digits=2),
	    "N:",
	    length(mydata[mydata[,"SAT"]=="NOAA-16","OBS"])
	)
	mystr17 <- paste("NOAA-17 | Mean:",
	    formatC(mean(
		mydata[mydata[,"SAT"]=="NOAA-17","OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-17","EST"],na.rm=T),
		format="f",digits=2),
	    "Median:",
	    formatC(median(
		mydata[mydata[,"SAT"]=="NOAA-17","OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-17","EST"],na.rm=T),
		format="f",digits=2),
	    "SD:",
	    formatC(sd(
		mydata[mydata[,"SAT"]=="NOAA-17","OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-17","EST"],na.rm=T),
		format="f",digits=2),
	    "N:",
	    length(mydata[mydata[,"SAT"]=="NOAA-17","OBS"])
	)
	mystr<-paste(mystr16,mystr17,sep="\n")
	text(0.5, min(mydata[,"OBS"]-mydata[,"EST"],na.rm=T),adj=c(0,0),mystr)
	title("All situations")
    } else if (method=="DSATO") { # Overcast
	plot(mydata[mydata[,"ST"]==0,"SAT"],
	    mydata[mydata[,"ST"]==0,"OBS"]-mydata[mydata[,"ST"]==0,"EST"],
	    xlab="Satellite",ylab="Observed-Estimated [W/m^2]")
	abline(0,0)
	mystr16 <- paste("NOAA-16 | Mean:",
	    formatC(mean(
		mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==0,"OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==0,"EST"],
		    na.rm=T),
		format="f",digits=2),
	    "Median:",
	    formatC(median(
		mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==0,"OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==0,"EST"],
		    na.rm=T),
		format="f",digits=2),
	    "SD:",
	    formatC(sd(
		mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==0,"OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==0,"EST"],
		    na.rm=T),
		format="f",digits=2),
	    "N:",
	    length(mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==0,"OBS"])
	)
	mystr17 <- paste("NOAA-17 | Mean:",
	    formatC(mean(
		mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==0,"OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==0,"EST"],
		    na.rm=T),
		format="f",digits=2),
	    "Median:",
	    formatC(median(
		mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==0,"OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==0,"EST"],
		    na.rm=T),
		format="f",digits=2),
	    "SD:",
	    formatC(sd(
		mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==0,"OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==0,"EST"],
		    na.rm=T),
		format="f",digits=2),
	    "N:",
	    length(mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==0,"OBS"])
	)
	mystr <- paste(mystr16,mystr17,sep="\n")
	text(0.5, min(mydata[mydata[,"ST"]==0,"OBS"]-
	    mydata[mydata[,"ST"]==0,"EST"],na.rm=T),adj=c(0,0),mystr)
	title("Overcast situations only (ST=0)")
    } else if (method=="DSATC") { # Clear sky
	plot(mydata[mydata[,"ST"]==60,"SAT"],
	    mydata[mydata[,"ST"]==60,"OBS"]-mydata[mydata[,"ST"]==60,"EST"],
	    xlab="Satellite",ylab="Observed-Estimated [W/m^2]")
	abline(0,0)
	mystr16 <- paste("NOAA-16 | Mean:",
	    formatC(mean(
		mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60,"OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60,"EST"],
		    na.rm=T),
		format="f",digits=2),
	    "Median:",
	    formatC(median(
		mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60,"OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60,"EST"],
		    na.rm=T),
		format="f",digits=2),
	    "SD:",
	    formatC(sd(
		mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60,"OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60,"EST"],
		    na.rm=T),
		format="f",digits=2),
	    "N:",
	    length(mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60,"OBS"])
	)
	mystr17 <- paste("NOAA-17 | Mean:",
	    formatC(mean(
		mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==60,"OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==60,"EST"],
		    na.rm=T),
		format="f",digits=2),
	    "Median:",
	    formatC(median(
		mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==60,"OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==60,"EST"],
		    na.rm=T),
		format="f",digits=2),
	    "SD:",
	    formatC(sd(
		mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==60,"OBS"]
		    -mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==60,"EST"],
		    na.rm=T),
		format="f",digits=2),
	    "N:",
	    length(mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==60,"OBS"])
	)
	mystr <- paste(mystr16,mystr17,sep="\n")
	text(0.5, min(mydata[mydata[,"ST"]==60,"OBS"]
	    -mydata[mydata[,"ST"]==60,"EST"],na.rm=T),adj=c(0,0),mystr)
	title("Clear sky situations only (ST=60)")
    } else if (method=="DSATOO") { # Overcast in both
	plot(mydata[mydata[,"ST"]==0&mydata[,"CM"]>tho,"SAT"],
	    mydata[mydata[,"ST"]==0&mydata[,"CM"]>tho,"OBS"]
		-mydata[mydata[,"ST"]==0&mydata[,"CM"]>tho,"EST"],
	    xlab="Satellite",ylab="Observed-Estimated [W/m^2]")
	abline(0,0)
	mystr16 <- paste("NOAA-16 | Mean:",
	    formatC(mean(
		mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==0&
		mydata[,"CM"]>tho,"OBS"]-mydata[mydata[,"SAT"]=="NOAA-16"&
		mydata[,"ST"]==0&mydata[,"CM"]>tho,"EST"],na.rm=T),
		format="f",digits=2),
	    "Median:",
	    formatC(median(
		mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==0&
		mydata[,"CM"]>tho,"OBS"]-mydata[mydata[,"SAT"]=="NOAA-16"&
		mydata[,"ST"]==0&mydata[,"CM"]>tho,"EST"],na.rm=T),
		format="f",digits=2),
	    "SD:",
	    formatC(sd(
		mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==0&
		mydata[,"CM"]>tho,"OBS"]-mydata[mydata[,"SAT"]=="NOAA-16"&
		mydata[,"ST"]==0&mydata[,"CM"]>tho,"EST"],na.rm=T),
		format="f",digits=2),
	    "N:",
	    length(mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==0&
		mydata[,"CM"]>tho,"OBS"])
	)
	mystr17 <- paste("NOAA-17 | Mean:",
	    formatC(mean(
		mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==0&
		mydata[,"CM"]>tho,"OBS"]-mydata[mydata[,"SAT"]=="NOAA-17"&
		mydata[,"ST"]==0&mydata[,"CM"]>tho,"EST"],na.rm=T),
		format="f",digits=2),
	    "Median:",
	    formatC(median(
		mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==0&
		mydata[,"CM"]>tho,"OBS"]-mydata[mydata[,"SAT"]=="NOAA-17"&
		mydata[,"ST"]==0&mydata[,"CM"]>tho,"EST"],na.rm=T),
		format="f",digits=2),
	    "SD:",
	    formatC(sd(
		mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==0&
		mydata[,"CM"]>tho,"OBS"]-mydata[mydata[,"SAT"]=="NOAA-17"&
		mydata[,"ST"]==0&mydata[,"CM"]>tho,"EST"],na.rm=T),
		format="f",digits=2),
	    "N:",
	    length(mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==0&
		mydata[,"CM"]>tho,"OBS"])
	)
	mystr<-paste(mystr16,mystr17,sep="\n")
	text(0.5, min(mydata[mydata[,"ST"]==0&mydata[,"CM"]>tho,"OBS"]-
	    mydata[mydata[,"ST"]==0&mydata[,"CM"]>tho,"EST"],na.rm=T),
	    adj=c(0,0),mystr)
	title(paste("Overcast situations only (ST=0 & CM >",tho,")"))
    } else if (method=="DSATCC") { # Clear sky in both
	plot(mydata[mydata[,"ST"]==60&mydata[,"CM"]<thc,"SAT"],
	    mydata[
	    mydata[,"ST"]==60&mydata[,"CM"]<thc,"OBS"]
	    -mydata[
	    mydata[,"ST"]==60&mydata[,"CM"]<thc,"EST"],
	    xlab="Satellite",ylab="Observed-Estimated [W/m^2]")
	abline(0,0)
	mystr16 <- paste("NOAA-16 | Mean:",
	    formatC(mean(
		mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60&
		mydata[,"CM"]<thc,"OBS"]-
		mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60&
		mydata[,"CM"]<thc,"EST"],na.rm=T),
		format="f",digits=2),
	    "Median:",
	    formatC(median(
		mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60&
		mydata[,"CM"]<thc,"OBS"]-
		mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60&
		mydata[,"CM"]<thc,"EST"],na.rm=T),
		format="f",digits=2),
	    "SD:",
	    formatC(sd(
		mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60&
		mydata[,"CM"]<thc,"OBS"]-
		mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60&
		mydata[,"CM"]<thc,"EST"],na.rm=T),
		format="f",digits=2),
	    "N:",
	    length(mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60&
	    mydata[,"CM"]<thc&mydata[,"CM"]>0,"OBS"])
	)
	mystr17 <- paste("NOAA-17 | Mean:",
	    formatC(mean(
		mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==60&
		mydata[,"CM"]<thc,"OBS"]-
		mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==60&
		mydata[,"CM"]<thc,"EST"],na.rm=T),
		format="f",digits=2),
	    "Median:",
	    formatC(median(
		mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==60&
		mydata[,"CM"]<thc,"OBS"]-
		mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==60&
		mydata[,"CM"]<thc,"EST"],na.rm=T),
		format="f",digits=2),
	    "SD:",
	    formatC(sd(
		mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==60&
		mydata[,"CM"]<thc,"OBS"]-
		mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==60&
		mydata[,"CM"]<thc,"EST"],na.rm=T),
		format="f",digits=2),
	    "N:",
	    length(mydata[mydata[,"SAT"]=="NOAA-17"&mydata[,"ST"]==60&
	    mydata[,"CM"]<thc,"OBS"])
	)
	mystr<-paste(mystr16,mystr17,sep="\n")
	text(0.5, min(mydata[mydata[,"ST"]==60&mydata[,"CM"]<thc&
	    mydata[,"CM"]>0,"OBS"]-mydata[mydata[,"ST"]==60&mydata[,"CM"]<thc&
	    mydata[,"CM"]>0,"EST"],na.rm=T),adj=c(0,0),mystr)
	title(paste("Clear sky situations only (ST=60 & CM<",thc,")"))
    } else if (method=="DST") {
	plot(mydata[,"ST"],mydata[,"OBS"]-mydata[,"EST"],
	    xlab="Direct insolation in minutes (observed)",
	    ylab="Observed-Estimated [W/m^2]")
	abline(0,0)
	mystr1 <- paste("N (total):",length(mydata[,"OBS"]))
	mystr2 <- paste("Mean cloudy:",
	    formatC(mean(
		mydata[mydata[,"ST"]==0,"OBS"]-mydata[mydata[,"ST"]==0,"EST"],
		na.rm=T,),format="f",digits=2),
		"N:",length(mydata[mydata[,"ST"]==0,"OBS"])
	)
	mystr3 <- paste("Mean clear:",
	    formatC(mean(mydata[mydata[,"ST"]==60,"OBS"]-
	    mydata[mydata[,"ST"]==60,"EST"],na.rm=T),format="f",digits=2),
	    "N:", length(mydata[mydata[,"ST"]==60,"OBS"])
	)
	mystr<-paste(mystr1,mystr2,mystr3,sep="\n")
	text(0,min(mydata[,"OBS"]-mydata[,"EST"],na.rm=T),adj=c(0,0),mystr)
    } else if (method=="DC") {
	plot(mydata[,"CM"],mydata[,"OBS"]-mydata[,"EST"],
	    xlab="Cloud cover from PPS (1: clear, 2: overcast)",
	    ylab="Observed-Estimated [W/m^2]")
	abline(0,0)
	mystr1 <- paste("N (total):",length(mydata[,"OBS"]))
	mystr2 <- paste("Mean cloudy:",
	    formatC(mean(
		mydata[mydata[,"CM"]>=tho,"OBS"]-mydata[mydata[,"CM"]>=tho,"EST"],
		na.rm=T,),format="f",digits=2),
		"N:",length(mydata[mydata[,"CM"]>=tho,"OBS"])
	)
	mystr3 <- paste("Mean clear:",
	    formatC(mean(mydata[mydata[,"CM"]<=thc,"OBS"]-
	    mydata[mydata[,"CM"]<=thc,"EST"],na.rm=T),format="f",digits=2),
	    "N:", length(mydata[mydata[,"CM"]<=thc,"OBS"])
	)
	mystr<-paste(mystr1,mystr2,mystr3,sep="\n")
	text(0,min(mydata[,"OBS"]-mydata[,"EST"],na.rm=T),adj=c(0,0),mystr)

    } else if (method=="DH") {
	myindex1 <- sapply(mydata[,"T.sat"],toString)
	myindex <- sapply(myindex1,substr,start=9,stop=12)
	myindex <- as.numeric(myindex)
	plot(myindex,mydata[,"OBS"]-mydata[,"EST"],
	    xlab="Hour (of sat.)",ylab="Observed-Estimated [W/m^2]")
	abline(0,0)
	mystr1 <- paste("Mean dif.:",
	    formatC(mean(mydata[,"OBS"]-mydata[,"EST"],na.rm=T),
		format="f",digits=2))
	mystr2 <- paste("Standard dev.:",
	    formatC(sd(mydata[,"OBS"]-mydata[,"EST"],na.rm=T),
		format="f",digits=2))
	mystr3 <- paste("N:",length(mydata[,"OBS"]))
	mystr<-paste(mystr1,mystr2,mystr3,sep="\n")
	text(min(myindex),
	    min(mydata[,"OBS"]-mydata[,"EST"],na.rm=T),adj=c(0,0),mystr)
    } else if (method=="POS") {
	boxplot((mydata[,"OBS"]-mydata[,"EST"]) ~ mydata[,"StId"],
	    xlab="Station",ylab="Observed-Estimated [W/m^2]",
	    type="p",las=2)
	title("All")
    } else if (method=="POSC") {
	boxplot((mydata[mydata[,"ST"]==60,"OBS"]-
	    mydata[mydata[,"ST"]==60,"EST"]) ~ 
	    mydata[mydata[,"ST"]==60,"StId"],
	    xlab="Station",ylab="Observed-Estimated [W/m^2]",
	    type="p",las=2)
	abline(h=0)
	title("Cloudfree")
    } else if (method=="POSCC") {
	boxplot((mydata[mydata[,"ST"]==60&mydata[,"CM"]<thc,"OBS"]-
	    mydata[mydata[,"ST"]==60&mydata[,"CM"]<thc,"EST"]) ~ 
	    mydata[mydata[,"ST"]==60&mydata[,"CM"]<thc,"StId"],
	    xlab="Station",ylab="Observed-Estimated [W/m^2]",
	    type="p",las=2)
	abline(h=0)
	title("Cloudfree (both)")
    } else if (method=="POSO") {
	boxplot((mydata[mydata[,"ST"]==0,"OBS"]-
	    mydata[mydata[,"ST"]==0,"EST"]) ~ 
	    mydata[mydata[,"ST"]==0,"StId"],
	    xlab="Station",ylab="Observed-Estimated [W/m^2]",
	    type="p",las=2)
	abline(h=0)
	title("Overcast")
    } else if (method=="POSOO") {
	boxplot((mydata[mydata[,"ST"]==60&mydata[,"CM"]>tho,"OBS"]-
	    mydata[mydata[,"ST"]==60&mydata[,"CM"]>tho,"EST"]) ~ 
	    mydata[mydata[,"ST"]==60&mydata[,"CM"]>tho,"StId"],
	    xlab="Station",ylab="Observed-Estimated [W/m^2]",
	    type="p",las=2)
	abline(h=0)
	title("Overcast (both)")
    } else if (method=="DTS") {
	mt <- sapply(mydata[,"T.sat"],toString)
	#mt <- sapply(mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60,"T.sat"],toString)
	year<-sapply(mt,substr,start=1,stop=4) 
	month<-sapply(mt,substr,start=5,stop=6) 
	day<-sapply(mt,substr,start=7,stop=8)
	hour<-sapply(mt,substr,start=9,stop=10)
	min<-sapply(mt,substr,start=11,stop=12)
	xdata <- ISOdatetime(year,month,day,hour,min,0)
	obs <- mydata[,"OBS"]
	est <- mydata[,"EST"]
	#obs <- mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60,"OBS"]
	#est <- mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60,"EST"]
	ydata <-  obs-est

	plot(xdata,ydata,
	    xlab="Time",ylab="Observed-Estimated [W/m^2]")
	abline(0,0)
	mystr1 <- paste(
	    "Mean obs.:",
	    formatC(mean(obs,na.rm=T),format="f",digits=2),
	    "N:",
	    formatC(length(obs),format="d"))
	mystr2 <- paste(
	    "Mean dif.:",
	    formatC(mean(ydata,na.rm=T),format="f",digits=2),
	    "SD:",
	    formatC(sd(ydata,na.rm=T),format="f"))
	mystr3 <- paste(
	    "RMSD:",
	    formatC(sqrt(mean(ydata^2,na.rm=T)),format="f",digits=2))
	mystr<-paste(mystr1,mystr2,mystr3,sep="\n")
	text(min(xdata,na.rm=T),
	    max(ydata,na.rm=T),adj=c(0,1),mystr)
	title("Timeseries - all data")
    } else if (method=="DTSC") {
	mt <- sapply(mydata[mydata[,"ST"]==60,"T.sat"],toString)
	#mt <- sapply(mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60,"T.sat"],toString)
	year<-sapply(mt,substr,start=1,stop=4) 
	month<-sapply(mt,substr,start=5,stop=6) 
	day<-sapply(mt,substr,start=7,stop=8)
	hour<-sapply(mt,substr,start=9,stop=10)
	min<-sapply(mt,substr,start=11,stop=12)
	xdata <- ISOdatetime(year,month,day,hour,min,0)
	obs <- mydata[mydata[,"ST"]==60,"OBS"]
	est <- mydata[mydata[,"ST"]==60,"EST"]
	#obs <- mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60,"OBS"]
	#est <- mydata[mydata[,"SAT"]=="NOAA-16"&mydata[,"ST"]==60,"EST"]
	ydata <-  obs-est

	plot(xdata,ydata,
	    xlab="Time",ylab="Observed-Estimated [W/m^2]")
	abline(0,0)
	mystr1 <- paste(
	    "Mean obs.:",
	    formatC(mean(obs,na.rm=T),format="f",digits=2),
	    "N:",
	    formatC(length(obs),format="d"))
	mystr2 <- paste(
	    "Mean dif.:",
	    formatC(mean(ydata,na.rm=T),format="f",digits=2),
	    "SD:",
	    formatC(sd(ydata,na.rm=T),format="f"))
	mystr3 <- paste(
	    "RMSD:",
	    formatC(sqrt(mean(ydata^2,na.rm=T)),format="f",digits=2))
	mystr<-paste(mystr1,mystr2,mystr3,sep="\n")
	text(min(xdata,na.rm=T),
	    max(ydata,na.rm=T),adj=c(0,1),mystr)
	title("Clear [ST=60]")
    } else if (method=="DTSCC") {
	mt <- sapply(mydata[mydata[,"ST"]==60&mydata[,"CM"]<thc,"T.sat"],toString)
	year<-sapply(mt,substr,start=1,stop=4) 
	month<-sapply(mt,substr,start=5,stop=6) 
	day<-sapply(mt,substr,start=7,stop=8)
	hour<-sapply(mt,substr,start=9,stop=10)
	min<-sapply(mt,substr,start=11,stop=12)
	xdata <- ISOdatetime(year,month,day,hour,min,0)
	obs <- mydata[mydata[,"ST"]==60&mydata[,"CM"]<thc,"OBS"]
	est <- mydata[mydata[,"ST"]==60&mydata[,"CM"]<thc,"EST"]
	ydata <-  obs-est

	plot(xdata,ydata,
	    xlab="Time",ylab="Observed-Estimated [W/m^2]")
	abline(0,0)
	mystr1 <- paste(
	    "Mean obs.:",
	    formatC(mean(obs,na.rm=T),format="f",digits=2),
	    "N:",
	    formatC(length(obs),format="d"))
	mystr2 <- paste(
	    "Mean dif.:",
	    formatC(mean(ydata,na.rm=T),format="f",digits=2),
	    "SD:",
	    formatC(sd(ydata,na.rm=T),format="f"))
	mystr3 <- paste(
	    "RMSD:",
	    formatC(sqrt(mean(ydata^2,na.rm=T)),format="f",digits=2))
	mystr<-paste(mystr1,mystr2,mystr3,sep="\n")
	text(min(xdata,na.rm=T),
	    max(ydata,na.rm=T),adj=c(0,1),mystr)
	title(paste("Clear sky situations only (ST=60 & CM<",thc,")"))
    } else if (method=="DTSO") {
	mt <- sapply(mydata[mydata[,"ST"]==0,"T.sat"],toString)
	year<-sapply(mt,substr,start=1,stop=4) 
	month<-sapply(mt,substr,start=5,stop=6) 
	day<-sapply(mt,substr,start=7,stop=8)
	hour<-sapply(mt,substr,start=9,stop=10)
	min<-sapply(mt,substr,start=11,stop=12)
	xdata <- ISOdatetime(year,month,day,hour,min,0)
	obs <- mydata[mydata[,"ST"]==0,"OBS"]
	est <- mydata[mydata[,"ST"]==0,"EST"]
	ydata <- obs-est 

	plot(xdata,ydata,
	    xlab="Time",ylab="Observed-Estimated [W/m^2]")
	abline(0,0)
	mystr1 <- paste(
	    "Mean obs.:",
	    formatC(mean(obs,na.rm=T), format="f",digits=2),
	    "N:",
	    formatC(length(obs),format="d"))
	mystr2 <- paste(
	    "Mean dif.:",
	    formatC(mean(ydata,na.rm=T),format="f",digits=2),
	    "SD:",
	    formatC(sd(ydata,na.rm=T),format="f"))
	mystr3 <- paste(
	    "RMSD:",
	    formatC(sqrt(mean(ydata^2,na.rm=T)),format="f",digits=2))
	mystr<-paste(mystr1,mystr2,mystr3,sep="\n")
	text(min(xdata,na.rm=T),
	    max(ydata,na.rm=T),adj=c(0,1),mystr)
	title("Overcast [ST=0]")
    } else if (method=="DTSOO") {
	mt <- sapply(mydata[mydata[,"ST"]==0&mydata[,"CM"]>=tho,"T.sat"],toString)
	year<-sapply(mt,substr,start=1,stop=4) 
	month<-sapply(mt,substr,start=5,stop=6) 
	day<-sapply(mt,substr,start=7,stop=8)
	hour<-sapply(mt,substr,start=9,stop=10)
	min<-sapply(mt,substr,start=11,stop=12)
	xdata <- ISOdatetime(year,month,day,hour,min,0)
	obs <- mydata[mydata[,"ST"]==0&mydata[,"CM"]>=tho,"OBS"]
	est <- mydata[mydata[,"ST"]==0&mydata[,"CM"]>=tho,"EST"]
	ydata <- obs-est 

	plot(xdata,ydata,
	    xlab="Time",ylab="Observed-Estimated [W/m^2]")
	abline(0,0)
	mystr1 <- paste(
	    "Mean obs.:",
	    formatC(mean(obs,na.rm=T), format="f",digits=2),
	    "N:",
	    formatC(length(obs),format="d"))
	mystr2 <- paste(
	    "Mean dif.:",
	    formatC(mean(ydata,na.rm=T),format="f",digits=2),
	    "SD:",
	    formatC(sd(ydata,na.rm=T),format="f"))
	mystr3 <- paste(
	    "RMSD:",
	    formatC(sqrt(mean(ydata^2,na.rm=T)),format="f",digits=2))
	mystr<-paste(mystr1,mystr2,mystr3,sep="\n")
	text(min(xdata,na.rm=T),
	    max(ydata,na.rm=T),adj=c(0,1),mystr)
	title(paste("Overcast situations only (ST=0 & CM >",tho,")"))
    } else if (method=="STCC") {
	plot(mydata[,"ST"],mydata[,"CM"],
	xlab="Minutes of direct insolation at station",
	ylab="Cloud cover index (1: clear, 2: overcast)")
    }


    if (printIt==TRUE) {
	dev.off()
    }
}