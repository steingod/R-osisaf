#
# PROGRAMMING LANGUAGE:
# S-language (for use in R)
#
# FUNCTION NAME:
# gammaPDFplot.sl
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
gammaPDFtableplot <- function(postscript=FALSE,basepath) {

    ##basepath<-"docs/conferences/IICWG-2001/"
    nofigs<-seq(1,9,1)
    params<-matrix(
	c(1.0,1.0,0.0,1.0,2.0,0.0,1.0,4.0,0.0,
	  1.5,1.0,0.0,1.5,2.0,0.0,1.5,4.0,0.0,
	  2.0,1.0,0.0,2.0,2.0,0.0,2.0,4.0,0.0),
	ncol=3,nrow=9,byrow=T)

    if (! postscript) {
	layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=T))
    }
    for (i in nofigs) {
	if (postscript) {
	    filename<-paste(basepath,"gpdf",i,".eps",sep="")
	    cat("Creating file: ",i,filename,"\n")
	}
	if (postscript) {
	    postscript(filename,onefile=F,horizontal=F,
		    paper="special",width=4,height=4)
	}
	gammaPDFplot(params[i,1],params[i,2],params[i,3])

	if (postscript) {
	    dev.off()
	}
    }

    return("You have reached the END!!")
}
