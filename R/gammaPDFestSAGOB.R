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
    if (method=="F") {
	gammaest <- gammaPDF3parest
    } else if (method=="G") {
	gammaest <- gammaPDF2parest
    } else if (method=="N") {
	normalpdf <- normalPDFest
    } else if (method=="A") {
	alpha <- as.numeric(readline("Please guess a value for alpha: "))
	beta <- as.numeric(readline("Please guess a value for beta: "))
	gamma <- as.numeric(readline("Please guess a value for gamma: "))
    } else {
	return("Could not chose gamma estimation method...")
    }

# Read data file
    dtostr<-monthnameconvert(file)
    cat("monthnameconvert OK\n")
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
	gpd<-gammaPDF(gpar,xpar)
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
