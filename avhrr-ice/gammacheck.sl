#
# PROGRAMMING LANGUAGE:
# S-language (for use in R)
#
# FUNCTION NAME:
# gammacheck.sl
#
# PURPOSE:
# To plot various versions of the gamma distribution using various
# parameter values...
#
# NOTES:
# NA
#
# BUGS:
# NA
#
# AUTHOR:
# Øystein Godøy, DNMI/FOU, 07/11/2001
#
function(alpha=1,beta=1,gamma=0,xl=c(0,10),yl=c(0,1),res=0.1) {

    gammapdf<-dget('/home/steingod/software/R-functions/osisaf/avhrr-ice/gammapdf.sl')

    gpar <- c(alpha,beta,gamma)
    xpar<-seq(xl[1],xl[2],res)
    gpd<-gammapdf(gpar,xpar)

    plot(xpar,gpd,type="l",col="blue",lwd=3,
	xlim=xl, ylim=yl,
	xlab="x",ylab="p(x)")

    subtxt<-paste("\\*a: ",format(gpar[1]),"\\*b: ",
	format(gpar[2]),"\\*g: ",format(gpar[3]))

#    maintxt<-paste(tmp$class,dtostr)
    text(xl[1],yl[2],subtxt,adj=0,vfont=c("serif","plain"))
#    title(main=maintxt,sub="Gamma Probability Density Function",
#	xlab=feature,ylab="Probability Density")
    return(NULL)
}
