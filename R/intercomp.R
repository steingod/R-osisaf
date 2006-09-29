#
# NAME:
# intercomp
#
# PURPOSE:
# To compare OSISAF Radiative Flux data produced at HL and LML.
#
# REQUIREMENTS:
# Files are ussually so large that scan sghould be used instead of
# read.table
#
# INPUT:
#
# OUTPUT:
#
# NOTES:
# DO NOT USE!!!! THIS IS YET NOT FINISHED!!!!
#
# BUGS:
#
# AUTHOR:
# Øystein Godøy, met.no/FOU, 16.06.2004 
#
# MODIFIED:
# NA
#
fluxintercomp <- function(dt="SSIHOUR",method="SLMLHL",mydata) {

    if (dt=="SSIHOUR") {
	file <-
	"/home/steingod/software/osisaf/support/complmlhl/collocated-ssi-hour.txt"
	d <- scan(file,
	    what=list(year=integer(0),month=integer(0),day=integer(0),
		hour=integer(0),minute=integer(0),
		sat=character(0),
		hlflux=double(0),hlbba=double(0),
		hlcl=integer(0),hlcc=integer(0),
		lmlflux=double(0),lmlbba=double(0),
		lmlcl=integer(0),lmlcc=integer(0),
		hlsoz=double(0),hlsaz=double(0),hlraz=double(0)))
    } else if (dt=="DLIHOUR") {
	file <-
	"/home/steingod/software/osisaf/support/complmlhl/collocated-dli-hour.txt"
	d <- scan(file,
	    what=list(year=integer(0),month=integer(0),day=integer(0),
		hour=integer(0),minute=integer(0),
		sat=character(0),
		hlflux=double(0),
		hlcl=integer(0),hlcc=integer(0),
		lmlflux=double(0),
		lmlcl=integer(0),lmlcc=integer(0)
		))
    } else if (dt=="SSIDAILY") {
	file <-
	"/home/steingod/software/osisaf/support/complmlhl/collocated-ssi-daily.txt"
	d <- scan(file,
	    what=list(year=integer(0),month=integer(0),day=integer(0),
		hour=integer(0),minute=integer(0),
		sat=character(0),
		hlflux=double(0),
		hlcl=integer(0),
		lmlflux=double(0),
		lmlcl=integer(0)
		))
    } else if (dt=="DLIDAILY") {
	file <-
	"/home/steingod/software/osisaf/support/complmlhl/collocated-dli-daily.txt"
	d <- scan(file,
	    what=list(year=integer(0),month=integer(0),day=integer(0),
		hour=integer(0),minute=integer(0),
		sat=character(0),
		hlflux=double(0),
		hlcl=integer(0),
		lmlflux=double(0),
		lmlcl=integer(0)
		))
    }
    


    mytime <- ISOdatetime(d$year,d$month,d$day,d$hour,d$minute,0)
    
    dflux <- d$hlflux-d$lmlflux
    dbba <- d$hlbba-d$lmlbba
    dfluxcl <- d$hlflux[d$hlcc==0&d$lmlcc==0]-d$lmlflux[d$hlcc==0&d$lmlcc==0]
    dbbacl <- d$hlbba[d$hlcc==0&d$lmlcc==0]-d$lmlbba[d$hlcc==0&d$lmlcc==0]
    dfluxoc <- d$hlflux[d$hlcc==2&d$lmlcc==2]-d$lmlflux[d$hlcc==2&d$lmlcc==2]
    dbbaoc <- d$hlbba[d$hlcc==2&d$lmlcc==2]-d$lmlbba[d$hlcc==2&d$lmlcc==2]

    cat("Fluxes\n")
    cat("  Bias:",
	formatC(mean(dflux),wid=6,dig=2,format="f"),"[W/m^2]")
    cat("  SD:",
	formatC(sd(dflux),wid=6,dig=2,format="f"),"[W/m^2]\n")

    if (dt=="SSIHOUR" || dt=="DLIHOUR") {
	cat("Clear sky\n")
	    cat("  Bias:",
		    formatC(mean(dfluxcl),wid=6,dig=2,format="f"),"[W/m^2]")
	    cat("  SD:",
		    formatC(sd(dfluxcl),wid=6,dig=2,format="f"),"[W/m^2]\n")

	    cat("Cloudy sky\n")
	    cat("  Bias:",
		    formatC(mean(dfluxoc),wid=6,dig=2,format="f"),"[W/m^2]")
	    cat("  SD:",
		    formatC(sd(dfluxoc),wid=6,dig=2,format="f"),"[W/m^2]\n")

	    if (dt=="SSIHOUR") {
		cat("BBA\n")
		    cat("  Bias:",
			    formatC(mean(dbba),wid=6,dig=2,format="f"),"[%]")
		    cat("      SD:",
			    formatC(sd(dbba),wid=6,dig=2,format="f"),"[%]\n")
		    cat("  Bias:",
			    formatC(mean(dbbacl),wid=6,dig=2,format="f"),"[%]")
		    cat("      SD:",
			    formatC(sd(dbbacl),wid=6,dig=2,format="f"),"[%]\n")
		    cat("  Bias:",
			    formatC(mean(dbbaoc),wid=6,dig=2,format="f"),"[%]")
		    cat("      SD:",
			    formatC(sd(dbbaoc),wid=6,dig=2,format="f"),"[%]\n")
	    }
    }

    tf <- factor(mytime)
    tm <- tapply(dflux,tf,mean)
    tsd <- tapply(dflux,tf,sd)
    if (dt=="SSIHOUR" || dt=="DLIHOUR") {
	tmcl <- tapply(dflux[d$hlcc==0&d$lmlcc==0],
		tf[d$hlcc==0&d$lmlcc==0],mean,na.rm=T)
	tsdcl <- tapply(dflux[d$hlcc==0&d$lmlcc==0],
		tf[d$hlcc==0&d$lmlcc==0],sd,na.rm=T)
	tmoc <- tapply(dflux[d$hlcc==2&d$lmlcc==2],
		tf[d$hlcc==2&d$lmlcc==2],mean,na.rm=T)
	tsdoc <- tapply(dflux[d$hlcc==2&d$lmlcc==2],
		tf[d$hlcc==2&d$lmlcc==2],sd,na.rm=T)
    }

    cat("\nStatistics after factor analysis:\n")
    cat(" Mean:",mean(tm,na.rm=T))
    if (dt=="SSIHOUR" || dt=="DLIHOUR") {
	cat(" ",mean(tmcl,na.rm=T),mean(tmoc,na.rm=T))
    }
    cat("\n")
    cat("   SD:",mean(tsd,na.rm=T))
    if (dt=="SSIHOUR" || dt=="DLIHOUR") {
	cat(" ",mean(tsdcl,na.rm=T),mean(tsdoc,na.rm=T))
    }
    cat("\n")

    layout(c(1,2))

    plot(strptime(levels(tf),format="%Y-%m-%d %H:%M:%S"),tm,
	ylab="Bias [W/m^2]")
    abline(h=c(-150,-75,0,75,150),col=c(2,4,1,4,2))
    plot(strptime(levels(tf),format="%Y-%m-%d %H:%M:%S"),tsd,
	ylab="SD [W/m^2]")
    abline(h=c(-150,-75,0,75,150),col=c(2,4,1,4,2))
    
    if (dt=="SSIHOUR" || dt=="DLIHOUR") {
	return(list(d=d,
	    tf=tf,tm=tm,tsd=tsd,tmcl=tmcl,tsdcl=tsdcl,tmoc=tmoc,tsdoc=tsdoc))
    } else {
	return(list(d=d,
	    tf=tf,tm=tm,tsd=tsd))
    }
}
