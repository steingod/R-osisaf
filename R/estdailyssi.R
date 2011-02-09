#
# PROGRAMMING LANGUAGE:
# S-language (for use in R)
#
# FUNCTION NAME:
# ssidaily.sl
#
# PURPOSE:
# To estimate the daily insolation using weights.
# 
# NOTES:
# Input is expected to be a data fram containing date and observations.
# The date information is expected to contain day of year along with
# standard date information. Day of the year (doy) in the range 1-365
#
# BUGS:
# This is not a generic function, it needs information on which year it is
# processing and more...
#
# AUTHOR:
# Øystein Godøy, DNMI/FOU, 21/11/2001
# MODIFIED:
# Øystein Godøy, DNMI/FOU, 27.06.2003
# Added equation of time...
# Øystein Godøy, DNMI/FOU, 27.01.2004
# Øystein Godøy, DNMI/FOU, 18.02.2004
# Modified Equation of Time. See equationoftime.sl for details.
#
function(mode=TRUE,clim=TRUE,dev=devgfissi,obs=obsgfissi,pos="GFI",esttz="UTC",s0sat=TRUE,plotit=TRUE) {

    # Include access to standard work space to reduce numer of commandline
    # items
    Sys.putenv("TZ"="GMT")
    globalenv()

    if (pos=="GFI"){
	lat <- 60.40
	lon <- 5.32
	obstz <- "TST"
	dev <- devgfissi
	obs <- obsgfissi
    } else if (pos=="SMHI") {
	lat <- 58.582
	lon <- 16.153
	##lat <- 58.35
	##lon <- 16.09
	obstz <- "UTC"
	dev <- devsmhissi
	obs <- obssmhissi
    } else if (pos=="KVITHAMAR") {
	lat <- 63.50
	lon <- 10.87
	obstz <- "UTC"
	dev <- devkvithamarssi
	obs <- obskvithamarssi
    } else if (pos=="FURENESET") {
	lat <- 61.30
	lon <- 5.05
	obstz <- "UTC"
	dev <- devfurenesetssi
	obs <- obsfurenesetssi
    } else if (pos=="SAERHEIM") {
	lat <- 58.78
	lon <- 5.68
	obstz <- "UTC"
	dev <- devsaerheimssi
	obs <- obssaerheimssi
    } else if (pos=="LANDVIK") {
	lat <- 58.33
	lon <- 8.52
	obstz <- "UTC"
	dev <- devlandvikssi
	obs <- obslandvikssi
    } else if (pos=="APELSVOLL") {
	lat <- 60.70
	lon <- 10.87
	obstz <- "UTC"
    } else {
	cat("This station name is not recognised.\n")
	return(NULL)
    }

    # Solar constant adapted for AVHRR response function
    if (s0sat) {
	s0 <- 1358.2
    } else {
	s0 <- 1367.
    }

    DEG2RAD <- pi/180

    # Create the necessary deviations of time specifications to be used in
    # the further processing.
    
    # First the correspondence between the time specification of
    # observations (anything among CET, UTC and TST) and estimates (UTC)
    # have to be clarified.
    
    # Local standard time (lst) is the normal winter time for the time
    # zone used (ie no summertime adjustment). True Solar Time (tst) is
    # the actual time in the position regardless time zone. Everything
    # should be converted to TST as this is required for estimation of the
    # clear sky irradiance. The relation between various time
    # specifications are:
    # True Solar Time = Local Standard Time + Equation of Time + Longitude
    # correction
    obstime <- obs$time
    esttime <- ISOdatetime(1970,1,1,0,0,0)+dev$time

    # Create True Solar Time without equation of time correction.
    if (obstz == "UTC") {
	##obstime <- obstime+(((lon-0.)/15.)*3600.)
	obstime <- obstime+((lon-0.)*240.)
    } else if (obstz == "CET") {
	##obstime <- obstime+(((lon-15.)/15.)*3600.)
	obstime <- obstime+((lon-15.)*240.)
    } else if (obstz == "TST") {
	cat("Observations already in TST\n")
    } else {
	return("Timezone not recognised for obstime")
    }
    if (esttz == "UTC") {
	##esttime <- esttime+(((lon-0.)/15.)*3600.)
	esttime <- esttime+((lon-0.)*240.)
    } else {
	return("Timezone not recognised for esttime")
    }

    # As the earth moves around the sun, solar time changes slightly with
    # respect to local standard time. (This is mainly related to the 
    # conservation of angular momentum as the earth moves around the sun.) 
    # This time difference is called the equation of time (et). 
    # This equation should be changed as it does not perfectly reproduce
    # the accurate corrections... The output of this equation is the
    # correction in hours - not minutes as usual...
    ##et <- 0.170*sin(4.*pi*(doy-80.)/373.)
	##-0.129*sin(2*pi*(doy-8.)/355.)
    # To an accuracy better than half a minute, results are in minutes...
    # Ref.: http://www.autodidacts.evesham.net/eot/
    doy <- as.numeric(strftime(as.POSIXlt(obstime),"%j"))
    ##theta <- doy*360./365.
    ##et <- -7.32*sin((-4.05+theta)*DEG2RAD)-10.08*sin((19.99+(2*theta)*DEG2RAD))
    ##et <- et*60.
    et <- 10.2*sin(4.*pi*((jd-80.)/373.))-7.74*sin(2*pi*((jd-8.)/355.))
    # Change base from minute to seconds
    et <- et*60.
    if (obstz != "TST") {
	obstime <- obstime+et
    }
    doy <- as.numeric(strftime(as.POSIXlt(esttime),"%j"))
    theta <- doy*360./365.
    et <- -7.32*sin((-4.05+theta)*DEG2RAD)-10.08*sin((19.99+(2*theta)*DEG2RAD))
    et <- et*60.
    esttime <- esttime+et

##    if (obstime=="UTC") {
##	tst <- lst+et+(lon-0.)/15.
##    } else if (obstime=="CET") {
##	tst <- lst+et+(lon-15.)/15.
##    } else if (obstime=="TST") {
##	tst <- lst
##    } else {
##	return("Timezone not recognised")
##    }

    # Decide which time (obs or est) that rules the processing. Generally,
    # obstime is only ruling when the method is tested on observations
    # only.
    if (mode) {
	cat("Checking methodology against observations only...\n")
	mytime <- obstime
    } else {
	mytime <- esttime
    }

    # Create day of year from input date specification
    doy <- as.numeric(strftime(as.POSIXlt(mytime),"%j"))
    month <- as.numeric(strftime(as.POSIXlt(mytime),"%m"))

    # Extract hour as decimal number from date specification.
    tst <- as.numeric(strftime(as.POSIXlt(mytime),"%H"))+
	(as.numeric(strftime(as.POSIXlt(mytime),"%M"))/60.)
##    if (lst < 0.) {
##	lst <- 23.99999 - lst
##    } else if (lst > 23.99999) {
##	lst <- lst-23.99999
##    }

    # Method from Rao and Chen (Paltridge and Platt, 1976) to estimate
    # variation in distance between Earth and Sun. This distance is at
    # minimum January 4 and maximum on July 5.
    # Iqbal, M 1983., Paltridge and Platt.
    theta0 <- 0.9836*(doy-1)*DEG2RAD
    # UO SRML (University of Oregon), the only difference being one day
    ##theta0 <- 2.*pi*(doy)
    esd <- (1.00011+0.03422*cos(theta0)+0.001280*sin(theta0)+
	0.000719*cos(2*theta0)+0.000077*sin(2*theta0))

    # Estimate declination (this is how the ~23.45 degrees angle between
    # the Earths axis and the rotation plane of the Earth around the Sun
    # is experienced at the Earth).
    decl <- 0.006918-0.399912*cos(theta0)+0.070257*sin(theta0)-
	0.006758*cos(2*theta0)+0.000907*sin(2*theta0)-
	0.002697*cos(3*theta0)+0.001480*sin(3*theta0)

    # Estimate hour angle. The hour angle is equivalent to the azimuthal
    # angle of the Sun relative to North. It is measured in the apparent
    # orbit of the Sun as it moves across the sky. It is zero at solar
    # noon (12h).
    ha <- (12.-tst)*(15.*DEG2RAD)

##    lst <- hours+(lon*0.0667)+et
##    cat(paste("Local time:",lt,"\n"))
##    cat(paste("UTC time:",hours,"\n"))
    
    # Estimate solar zenith angle
    coszensun <- sin(decl)*sin(lat*DEG2RAD)+
	cos(decl)*cos(lat*DEG2RAD)*cos(ha)
##    if (abs(coszensun) > 1.0) {
##	if (coszensun >= 0.0) {
##	    coszensun <- 1.0
##	} else {
##	    coszensun <- -1.0
##	}
##    }
    zensun <- acos(coszensun)/DEG2RAD
    elesun <- 90.-(zensun/DEG2RAD)
    
    # Estimate extraterrestric irradiance
    etr <- s0*coszensun*esd

    # Estimate weights to apply
    ##crew <- dget("/home/faog/software/R-functions/ssi/weightsdaily.sl")
    crew <- dget(paste(Sys.getenv("HOME"),
	"/software/R-functions/ssi/weightsdaily.sl",sep=""))
    weights <- crew(s0, lat, 2001, doy, ha, esd, decl, clim, dev$pw, dev$ps)

    # Modify observations, get mean daily observations assuming full
    # coverage throughout the day...
    if (mode) {
	myobs <- data.frame(time=mytime,doy=doy,ssi=obs$d)
	meanobs <- tapply(myobs$ssi,factor(myobs$doy),sum,na.rm=T)
	nobs <- tapply(myobs$ssi,factor(myobs$doy),length)
	meanobs <- meanobs/nobs
    } else {
	dev$ssi <- ifelse(dev$ssi<0,NA,dev$ssi)
	myobs <- data.frame(time=mytime,doy=doy,ssi=dev$ssi)
	odoy <- as.numeric(strftime(as.POSIXlt(obstime),"%j"))
	meanobs <- tapply(obs$d,factor(odoy),sum,na.rm=T)
	nobs <- tapply(obs$d,factor(odoy),length)
	meanobs <- meanobs/nobs
    }
    meanobs <- ifelse(nobs < 23, NA, meanobs)

    # Apply weights to estimates and compute mean daily values from
    # estimates...
    ki <- weights$kibase*myobs$ssi
    ##wessi <- weights$mgloc[doy]*weights$wi*ki
    wessi <- weights$mgloc[doy]*ki
    meanest <- tapply(wessi,factor(doy),mean,na.rm=T)
    nest <- tapply(wessi,factor(doy),length)

    if (mode) {
	cat("Statistics:\n")
	cat(paste("\t Bias:",mean(meanobs-meanest,na.rm=T),"\n"))
	cat(paste("\tStDev:",sd(meanobs-meanest,na.rm=T),"\n"))
	cat(paste("\t RMSD:",sqrt(mean((meanobs-meanest)^2.,na.rm=T)),"\n"))

	if (plotit) {
	    plot(meanobs-meanest,xlab="Day of year");abline(h=0)
	    
	    title(pos)
	    cat("Select upper right corner for legend\n")
	    text(locator(),
		paste(
		" Bias:",round(mean(meanobs-meanest,na.rm=T),d=2),
		    "W/m^2\n",
		"StDev:",round(sd(meanobs-meanest,na.rm=T),d=2),
		    "W/m^2\n",
		" RMSD:",round(sqrt(mean((meanobs-meanest)^2.,
		    na.rm=T)), d=2), "W/m^2"),
	    adj=1)
	}
    } else {
	i <- match(as.numeric(levels(factor(doy))),
	    as.numeric(levels(factor(odoy))))
	nmo <- meanobs[i]
	meanobs <- nmo

	cat("Statistics:\n")
	cat(paste("\t Bias:",round(mean(meanobs-meanest,na.rm=T),d=2),"\n"))
	cat(paste("\tStDev:",round(sd(meanobs-meanest,na.rm=T),d=2),"\n"))
	cat(paste("\t RMSD:",round(sqrt(mean((meanobs-meanest)^2.,na.rm=T)),
	    d=2),"\n"))

	if (plotit) {
	    plot(i,meanobs-meanest,xlab="Day of year");abline(h=0)
	    
	    title(pos)
	    cat("Select upper right corner for legend\n")
	    text(locator(),
		paste(
		" Bias:",round(mean(meanobs-meanest,na.rm=T),d=2),
		    "W/m^2\n",
		"StDev:",round(sd(meanobs-meanest,na.rm=T),d=2),
		    "W/m^2\n",
		" RMSD:",round(sqrt(mean((meanobs-meanest)^2.,
		    na.rm=T)), d=2), "W/m^2"),
	    adj=1)
	}
    }

    return(list(time=mytime,doy=doy,month=month,
	esd=esd,decl=decl,
	et=et,tst=tst,ha=ha,
	soz=zensun,
	etr=etr,
	wi=weights$wi,ki=ki,
	metr=weights$metr,mgloc=weights$mgloc,
	mobs=meanobs,mest=meanest,
	wssi=wessi,
	ssi=myobs$ssi,
	cgloc=weights$cgloc,wmonth=weights$wmonth,
	tr=weights$tr,cetr=weights$cetr,
	ossi=obs$d,essi=dev$ssi,
	nobs=nobs,nest=nest))
}
