#
# NAME:
#
# PURPOSE:
#
# REQUIREMENTS:
#
# INPUT:
#
# OUTPUT:
#
# NOTES:
#
# BUGS:
#
# AUTHOR:
# Øystein Godøy, METNO/FOU, 18.05.2006 
#
# MODIFIED:
# NA 

compprocchains <- function(chain1,chain2,
	from=ISOdate(1996,1,1),to=Sys.time(),area=NULL) {

    list1 <- list.files(path=chain1)
    
    if (any(grep("dli",chain1))) {
	myylab <- "Differences of means [W/m^2]"
	myproduct <- ""
    } else if (any(grep("ssi",chain1))) {
	myylab <- "Differences of means [W/m^2]"
    } else if (any(grep("sst",chain1))) {
	myylab <- "Differences of means [K]"
    } else if (any(grep("ice",chain1))) {
	if (any(grep("conc",chain1))) {
	    myylab <- "Differences of means [%]"
	    myproduct <- "Sea Ice Concentration"
	} else if (any(grep("edge",chain1))) {
	    myylab <- "Differences of means"
	    myproduct <- "Sea Ice Edge"
	} else if (any(grep("type",chain1))) {
	    myylab <- "Differences of means"
	    myproduct <- "Sea Ice Type"
	} else {
	    cat("Could not determine product type.\n")
	    return(NULL)
	}
    } else {
	cat("Could not determine product type.\n")
	return(NULL)
    }
    mysubtitle <- paste("Time period:",from,"to",to)

    mvalues <- numeric()
    mtimes_tmp <-vector()


    for (fname in list1) {
	fname1 <- paste(chain1,fname,sep="/")
	fname2 <- paste(chain2,fname,sep="/")
	if (length(grep("hdf",fname1,fixed=TRUE))==0) {
	    next
	}
	if (! is.null(area)) {
	    if (! any(grep(area,fname1,perl=TRUE))) {
		next
	    }
	}
	if (difftime(file.info(fname1)$mtime,as.POSIXct(from),units="secs") <0) {
	    cat(paste("Skipping file:",fname,", too old\n"))
	    next
	}
	if (difftime(file.info(fname1)$mtime,as.POSIXct(to),units="secs")
	    > 0) {
	    cat(paste("Skipping file:",fname,", too new\n"))
	    next
	}
	if (!file.exists(fname1)) {
	    next
	}
	if (!file.exists(fname2)) {
	    next
	}
	d1 <- readosisaf(file=fname1)
	d2 <- readosisaf(file=fname2)
	timeid1 <- ISOdatetime(d1$header$year,d1$header$month,
		d1$header$day,d1$header$hour,d1$header$minute,0,"GMT")
	timeid2 <- ISOdatetime(d2$header$year,d2$header$month,
		d2$header$day,d2$header$hour,d2$header$minute,0,"GMT")
	if (difftime(timeid1,timeid2,units="secs") != 0) {
	    next
	}

	mvalues[length(mvalues)+1] <- mean(d1$data,na.rm=T)-mean(d2$data,na.rm=T)
	mtimes_tmp[length(mtimes_tmp)+1] <- timeid1
    }

    mtimes <- ISOdatetime(1970,1,1,0,0,0)+mtimes_tmp

    plot(mtimes,mvalues,type="b",ylab=myylab,xlab="")
    abline(h=0)
    title("Difference plot between first and second chain",sub=mysubtitle)

    return(data.frame(time=mtimes,data=mvalues))
}
