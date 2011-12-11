#
# Read OSISAF HDF5 files.
# 
# It is assumed that all fields ahre the same datatype which is not the
# case for some datasets, thus the default is to read only the first
# datalayer.
#
# $Id: readosisaf.R,v 1.4 2011-12-11 20:12:55 steingod Exp $
#
readosisaf <- function(filename,layers=1,nomiss=TRUE) {

    if (missing(filename)) {
	cat("Remember to specify filename\n")
	return;
    }
    
    description <- character(length=1)
    #projection <- character(length=1)
    year <- integer(length=1)
    month <- integer(length=1)
    day <- integer(length=1)
    hour <- integer(length=1)
    minute <- integer(length=1)
    xsize <- integer(length=1)
    ysize <- integer(length=1)
    zsize <- integer(length=1)
    ucs_ul_x <- double(length=1)
    ucs_ul_y <- double(length=1)
    ucs_dx <- double(length=1)
    ucs_dy <- double(length=1)
    
    header <- .C("readosisafheader",
	filename=as.character(filename),description=as.character(description),
	year=as.integer(year),month=as.integer(month),day=as.integer(day),
	hour=as.integer(hour),minute=as.integer(minute),
	xsize=as.integer(xsize),ysize=as.integer(ysize),zsize=as.integer(zsize),
	ucs_ul_x=as.double(ucs_ul_x),ucs_ul_y=as.double(ucs_ul_y),
	ucs_dx=as.double(ucs_dx),ucs_dy=as.double(ucs_dy),
	package="osisaf")

    if (layers=="all") {
        size <- header$xsize*header$ysize
    } else {
        size <- header$xsize*header$ysize
        header$zsize <- layers
    }
    #mydata <- vector(mode="numeric",length=size)
    mydata <- array(0,dim=c(size,header$zsize))
    
    tmp <- .C("readosisafdata",
	filename=as.character(filename),
	data=mydata,package="osisaf")
    if (nomiss) {
	tmp$data[tmp$data < -50] <- NA
    }

    return(list(header=header,data=tmp$data))
}
