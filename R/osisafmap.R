osisafmap <- function(data, linjer = pretty(vektor, 10.), plotdata=FALSE, otext = "", utext = "") {

    data(osisafmapdata)
#
# Interpolate vector data and plot them on the EMEP map of
# the North Atlantic. Data is assumed to come from the
# Hindcast database.
#
# Create datamatrix that is convenient for plotting. That is
# get coordinatesystem information.
#
    if (plotdata == TRUE) {
	if (length(vektor) != length(koord.hc[, 1.])) {
	    stop("You have to interpolate Your data to DNMI Hindcast grid before You proceede!")
	    }
    }
#
# Interpolate grid data.
#
    if (plotdata == TRUE) {
	matrise <- cbind(koord.hc[, 1.], koord.hc[, 2.], vektor)
    }
#
# Initialize plotting area for figure to retain geometric correct
# impression of the map.
#
    if (plotdata == TRUE) {
	interpolert <- 	interp(matrise[, 1.], matrise[, 2.], matrise[, 3.])
    }
# Draw the underlying map and meridians.
#
#
#    par(fin = c(4.125, 5.5))
# Draw contours of the data analysed in this call.
#
#
#plot(kart.hc[, 1.], kart.hc[, 2.], type = "l", xlab = "", ylab = "", axes = F)
    plot(kartkoord[, 1.], kartkoord[, 2.], type = "l", 
    xlab = "", ylab = "", axes = T,xlim=c(13,19),ylim=c(19,34))
# Draw a frame around the plot.
#
#
	if (plotdata == TRUE) {
	    contour(interpolert, levels = linjer, add = T, col = 3., labex = 0.5,
		    lty = 1., col = 1.)
	}
# Print title and subtitle.
#
#
    box()
	title(otext, cex = 0.6)
	if(utext == "date")
	    title(sub = paste("Plot created: ", date()), cex = 0.6)
	else if(nchar(utext) > 0.)
	    title(sub = utext, cex = 0.6)
}
