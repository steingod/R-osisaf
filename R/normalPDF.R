normalPDF <- function(mean, sdev, xpar) {

    prob <- (1/(sqrt(2*pi)*sdev))*(exp(-1*((xpar-mean)^2/(2*sdev^2))))

    return(data.frame(x=xpar,y=prob))
}
