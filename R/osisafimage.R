#
# $Id: osisafimage.R,v 1.3 2011-11-03 19:27:16 steingod Exp $
#
osisafimage <- function(dataset, layer=1, map=FALSE, ...) {

    if (missing(dataset)) {
	cat("Remember to provide an object from readosisaf.\n")
	return;
    }

    eastings <- dataset$header$ucs_ul_x+
	(dataset$header$ucs_dx*(0:(dataset$header$xsize-1)))
    northings <- dataset$header$ucs_ul_y-
	(dataset$header$ucs_dy*(0:(dataset$header$ysize-1)))

    eastings <- sort(eastings)
    northings <- sort(northings)


    t <- matrix(dataset$data[,layer],
	    ncol=dataset$header$ysize,nrow=dataset$header$xsize)

    aspectratio <- dataset$header$ysize/dataset$header$xsize
    par(fin=c(5,5*aspectratio))

    ##image(eastings,northings,t[,dataset$header$ysize:1])

    if (map==TRUE) {
	data(osisafmapdata)
        mapdata <- latlon2ucs(osisafmapdata$lat,osisafmapdata$lon)
        ##lines(mapdata$eastings,mapdata$northing)
	filled.contour(eastings,northings,t[,dataset$header$ysize:1],
		asp=aspectratio,
		plot.axes={axis(1);axis(2);
		lines(mapdata$eastings,mapdata$northing)},
		color.palette=topo.colors, ...)
    } else {
	filled.contour(eastings,northings,t[,dataset$header$ysize:1],
		asp=aspectratio,color.palette=topo.colors, ...)
    }
    
    title(paste(dataset$header$description),
    sub=paste(paste(dataset$header$year,formatC(dataset$header$month,width=2,flag="0"),formatC(dataset$header$day,width=2,flag="0"),sep="-"),
    paste("Layer:",layer),sep="\n"))

}
