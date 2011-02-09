#
# PROGRAMMING LANGUAGE:
# S-language (for use in R)
#
# FUNCTION NAME:
# weightsdaily.sl
#
# PURPOSE:
# To estimate the daily weights to be applied when averaging irregular
# SSI data to a daily value.
# 
# NOTES:
# NA
#
# BUGS:
# This is not a generic function, it needs information on which year it is
# processing and more...
#
# AUTHOR:
# Øystein Godøy, DNMI/FOU, 20.01.2004
#
# MODIFIED:
# Øystein Godøy, DNMI/FOU, 27.01.2004
# Temporary fix to base weights on surface clear sky irradiance instead of
# TOA irradiance. At present only climatological values are used, this
# will have to be fixed...
#
function(s0, lat, myyear, doy, cha, esd, decl, clim, pw, ps) {

    DEG2RAD <- pi/180
    sozcut <- 80

    if (clim==FALSE) {
	vdoy <- as.numeric(levels(factor(doy)))
	meanpw <- tapply(pw,factor(doy),mean,na.rm=T)
	meanps <- tapply(ps,factor(doy),mean,na.rm=T)
	meanpf <- meanps/1013.25
	pf <- ps/1013.25
    }

    gfipf <- rep(1,12)
    gfipw <- c(0.80, 0.80, 0.80, 1.00, 1.30, 1.60, 1.90, 1.90, 1.60, 1.30,
	1.00, 0.90)
    gfio3 <- c(0.33, 0.39, 0.42, 0.40, 0.39, 0.36, 0.34, 0.32, 0.30, 0.28,
	0.30, 0.31)
    gfialbs <- c(0.25, 0.25, 0.20, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15,
	0.15, 0.20)

    tst <- seq(0.0,23.9833333,0.01666667)

    ha <- (12.-tst)*(15.*DEG2RAD)

    coszensun <- numeric(length(tst))
    etr <- numeric(length(tst))
    metr <- numeric(365)
    mgloc <- numeric(365)
    tesd <- numeric(365)
    tdecl <- numeric(365)

    ##vdoys <- match(as.numeric(levels(factor(doy))),seq(1,365,1))
    ##k <- 1
    for (i in 1:365) {
	month <- as.numeric(
	    strftime(as.POSIXlt(ISOdate(myyear,1,1)+(86400*(i-1))),"%m"))

	theta0 <- 0.9836*(i-1)*DEG2RAD
	tesd[i] <- (1.00011+0.03422*cos(theta0)+0.001280*sin(theta0)+
	    0.000719*cos(2*theta0)+0.000077*sin(2*theta0))
	tdecl[i] <- 0.006918-0.399912*cos(theta0)+0.070257*sin(theta0)-
	    0.006758*cos(2*theta0)+0.000907*sin(2*theta0)-
	    0.002697*cos(3*theta0)+0.001480*sin(3*theta0)+
	    0.000077*sin(2*theta0)
	coszensun <- sin(tdecl[i])*sin(lat*DEG2RAD)+
	    cos(tdecl[i])*cos(lat*DEG2RAD)*cos(ha)
	etr <- s0*coszensun*tesd[i]

	odo3 <- 0.038*(gfio3[month]^0.44);
	if (clim) {
	    odo2 <- 0.0075*(gfipf[month]^0.87);
	    odco2 <- 0.0076*(gfipf[month]^0.29);
	    odsc <- 0.038*gfipf[month];
	    odpw <- 0.104*(gfipw[month]^0.3);
	    oda <- 0.007+0.009*gfipw[month];
	} else {
	    if (is.element(i,vdoy)) {
		odo2 <- 0.0075*(meanpf[vdoy == i]^0.87);
		odco2 <- 0.0076*(meanpf[vdoy == i]^0.29);
		odsc <- 0.038*meanpf[vdoy == i];
		odpw <- 0.104*(meanpw[vdoy == i]^0.3);
		oda <- 0.007+0.009*meanpw[vdoy == i];
	    } else {
		odo2 <- 0.0075*(gfipf[month]^0.87);
		odco2 <- 0.0076*(gfipf[month]^0.29);
		odsc <- 0.038*gfipf[month];
		odpw <- 0.104*(gfipw[month]^0.3);
		oda <- 0.007+0.009*gfipw[month];
	    }
	}

	od0 <- odo3+odpw+odo2+odco2+odsc+oda;
	ns <- 1.1-2.*od0;
	od <- od0*((1./coszensun)^ns);
	if (clim) {
	    tr <- exp(-od)*(1.+0.065*gfipf[month]*gfialbs[month]);
	} else {
	    if (is.element(i,vdoy)) {
		tr <- exp(-od)*(1.+0.065*meanpf[vdoy == i]*gfialbs[month]);
	    } else {
		tr <- exp(-od)*(1.+0.065*gfipf[month]*gfialbs[month]);
	    }
	}

	gloc <- etr*tr;
	#cat(paste(etr,gloc,tr,"\n"))

	##metr[i] <- mean(etr[etr>0],na.rm=T)
	metr[i] <- (sum(etr[etr>0],na.rm=T)/length(etr))
	##mgloc[i] <- mean(gloc[gloc>0],na.rm=T)
	mgloc[i] <- (sum(gloc,na.rm=T)/length(gloc))
    }

    cczs <- sin(decl)*sin(lat*DEG2RAD)+
	cos(decl)*cos(lat*DEG2RAD)*cos(cha)
    cetr <- s0*cczs*esd

    month <- as.numeric(
	strftime(as.POSIXlt(ISOdate(myyear,1,1)+(86400*(doy-1))),"%m"))
    odo3 <- 0.038*(gfio3[month]^0.44);
    if (clim) {
	odpw <- 0.104*(gfipw[month]^0.3);
	oda <- 0.007+0.009*gfipw[month];
	odo2 <- 0.0075*(gfipf[month]^0.87);
	odco2 <- 0.0076*(gfipf[month]^0.29);
	odsc <- 0.038*gfipf[month];
    } else {
	odpw <- 0.104*(pw^0.3);
	oda <- 0.007+0.009*pw;
	odo2 <- 0.0075*(pf^0.87);
	odco2 <- 0.0076*(pf^0.29);
	odsc <- 0.038*pf;
    }

    od0 <- odo3+odpw+odo2+odco2+odsc+oda;
    ns <- 1.1-2.*od0;
    od <- od0*((1./cczs)^ns);
    if (clim) {
	tr <- exp(-od)*(1.+0.065*gfipf[month]*gfialbs[month]);
    } else {
	tr <- exp(-od)*(1.+0.065*pf*gfialbs[month]);
    }

    cgloc <- cetr*tr;

    kibase <- 1./cgloc
    kibase <- ifelse(acos(cczs)/(pi/180.) < sozcut, kibase,NA)
    ##weights <- ifelse(acos(cczs)/(pi/180.) < sozcut, metr[doy]/cetr,NA)
    ##weights <- (acos(cczs)/(pi/180.))/45.
    ##weights <- ifelse(weights>2.5,NA,weights)
    ##weights <- weights-0.05
    ##wi <- ifelse(acos(cczs)/(pi/180.) < sozcut, mgloc[doy]/cgloc,NA)
    wi <- ifelse(acos(cczs)/(pi/180.) < sozcut, cgloc/mgloc[doy],NA)

    return(list(wi=wi,kibase=kibase,metr=metr,mgloc=mgloc,cgloc=cgloc,wmonth=month,tr=tr,cetr=cetr))
}
