#
# PROGRAMMING LANGUAGE:
# S-language (for use in R)
#
# FUNCTION NAME:
# gammacheck.sl
#
# PURPOSE:
# To plot several figures containing the Gamma distribution.
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

    gammacheck<-dget('/home/steingod/software/R-functions/osisaf/avhrr-ice/gammacheck.sl')

    basepath<-"docs/conferences/IICWG-2001/"
    nofigs<-seq(1,9,1)
    params<-matrix(
	c(1.0,1.0,0.0,1.0,2.0,0.0,1.0,4.0,0.0,
	  1.5,1.0,0.0,1.5,2.0,0.0,1.5,4.0,0.0,
	  2.0,1.0,0.0,2.0,2.0,0.0,2.0,4.0,0.0),
	ncol=3,nrow=9,byrow=T)

    for (i in nofigs) {
	filename<-paste(basepath,"gpdf",i,".eps",sep="")
	cat("Creating file: ",i,filename,"\n")
	postscript(filename,onefile=F,horizontal=F,
	    paper="special",width=4,height=4)
	gammacheck(params[i,1],params[i,2],params[i,3])
#	readline("Push any key to continue...")
	dev.off()
    }

    return("You have reached the END!!")
}
