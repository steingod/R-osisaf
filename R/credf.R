credf <- function(mydata) {
    # Convert matrix to vector
    #var1 <- as.vector(mydata$data)

    idx <- seq(0,mydata$header$xsize-1,1)
    idy <- seq(0,mydata$header$ysize-1,1)
    idmx <- rep(idx,mydata$header$ysize)
    idmy <- rep(idy,mydata$header$xsize)

    # Estimate positions for each pixel
    ucsx <- mydata$header$ucs_ul_x+(mydata$header$ucs_dx*idmx)
    ucsy <- mydata$header$ucs_ul_y-(mydata$header$ucs_dy*idmy)

    df <- data.frame(x=ucsx,y=ucsy,var1)

    return(df)
}
