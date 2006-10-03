gammaPDFplot <- function(alpha=1,beta=1,gamma=0,xl=c(0,10),yl=c(0,1),res=0.1) {

    gpar <- c(alpha,beta,gamma)
    xpar<-seq(xl[1],xl[2],res)
    gpd<-gammapdf(gpar,xpar)

    plot(xpar,gpd,type="l",col="blue",lwd=3,
	xlim=xl, ylim=yl,
	xlab="x",ylab="p(x)")

    subtxt<-paste("\\*a: ",format(gpar[1]),"\\*b: ",
	format(gpar[2]),"\\*g: ",format(gpar[3]))

    text(xl[1],yl[2],subtxt,adj=0,vfont=c("serif","plain"))

    return(NULL)
}
