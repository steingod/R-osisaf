#
# PROGRAMMING LANGUAGE:
# S-language (for use in R)
#
# FUNCTION NAME:
# gammaeda.sl
#
# PURPOSE:
# To read a dataset created by SAGOB->anabase, estimate three-parameter
# gamma distribution parameters, echo these to screen and plot both
# probability histograms as well as fitted gamma or normal function to 
# the dataset.
#
# NOTES:
# NA
#
# BUGS:
# NA
#
# AUTHOR:
# Øystein Godøy, DNMI/FOU, 13/06/2001
#
function(file,feature="CH2",ylim=c(0,1),method="F",resolution=1) {

# Load required functions
    readSAGOB<-dget("software/R-functions/osisaf/avhrr-ice/readSAGOB.sl")
    if (method=="F") {
	gammaest<-dget("software/R-functions/osisaf/avhrr-ice/gammaest.sl")
    } else if (method=="G") {
	gammaest<-dget("software/R-functions/osisaf/avhrr-ice/gammaest2.sl")
    } else if (method=="N") {
	normalpdf<-dget("software/R-functions/osisaf/avhrr-ice/normalpdf.sl")
    } else if (method=="A") {
	alpha <- as.numeric(readline("Please guess a value for alpha: "))
	beta <- as.numeric(readline("Please guess a value for beta: "))
	gamma <- as.numeric(readline("Please guess a value for gamma: "))
    } else {
	return("Could not chose gamma estimation method...")
    }
    gammapdf<-dget("software/R-functions/osisaf/avhrr-ice/gammapdf.sl")
    monthconv<-dget("software/R-functions/osisaf/avhrr-ice/monthconv.sl")

# Read data file
    dtostr<-monthconv(file)
    cat("monthconv OK\n")
    tmp<-readSAGOB(file)
    cat("readSAGOB OK\n")
    d<-dim(tmp$data)

# Estimate three parameter gamma distribution parameters
    if (method=="A") {
	gpar<-c(alpha,beta,gamma)
    } else if (method=="G" || method=="F") {
	gpar<-gammaest(tmp$data[,feature])
	cat("gammaest OK\n")
    }

# Estimate gamma PDF for plotting
    cat("Dataset characteristics (min, max, resolution): ")
    cat(min(tmp$data[,feature],na.rm=T),
	max(tmp$data[,feature],na.rm=T),
	resolution,"\n")
    xpar<-seq(min(tmp$data[,feature],na.rm=T),
	max(tmp$data[,feature],na.rm=T),
	resolution)
    if (method=="N") {
	gpd<-normalpdf(tmp$data[,feature])
    } else {
	gpd<-gammapdf(gpar,xpar)
    }

# Do the actual plotting
    
    par(pty="s")

    hist(tmp$data[,feature],probability=T,
	xlim=c(min(tmp$data[,feature],na.rm=T),max(tmp$data[,feature],na.rm=T)),
	ylim=ylim,border="yellow",col="blue",main="",xlab="",ylab="")

    par(new=T)
    if (method=="N") {
	plot(tmp$data[,feature],gpd,type="l",col="red",lwd=3,
	    xlim=c(min(tmp$data[,feature],na.rm=T),
	    max(tmp$data[,feature],na.rm=T)),
	    ylim=ylim,xlab="",ylab="")
    } else {
	plot(xpar,gpd,type="l",col="red",lwd=3,
	    xlim=c(min(tmp$data[,feature],na.rm=T),
	    max(tmp$data[,feature],na.rm=T)),
	    ylim=ylim,xlab="",ylab="")

	subtxt<-paste("\\*a: ",format(gpar[1]),
	    "\\*b: ",format(gpar[2]),
	    "\\*g: ",format(gpar[3]),"N: ",d[1])

	maintxt<-paste(tmp$class,dtostr)
	text(min(tmp$data[,feature],na.rm=T),ylim[2],
	    subtxt,adj=0,vfont=c("serif","plain"))

	title(main=maintxt,sub="Gamma Probability Density Function",
	    xlab=feature,ylab="Probability Density")
    }


    return("Jippppiiiiiiiii!!")
}
