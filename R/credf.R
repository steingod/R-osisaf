# Adapt data read by readosisaf to a form suitable for plotting in
# ggplot2. I.e. transform to a data.frame
credf <- function(mydata,layer=1) {

    idx <- seq(0,mydata$header$xsize-1,1)
    #idy <- seq(mydata$header$ysize-1,0,-1)
    idy <- seq(0,mydata$header$ysize-1,1)

    # Convert matrix to vector
    mymatrix <- matrix(mydata$data[,layer],
                       ncol=mydata$header$ysize,
                       nrow=mydata$header$xsize,
                       byrow=F)
    #flux <- as.vector(t(mymatrix[,mydata$header$ysize:1]))
    #flux <- as.vector(mymatrix)
    flux <- as.vector(mydata$data[,layer])

    idmx <- rep(idx,mydata$header$ysize)
    idmy <- rep(idy,each=mydata$header$xsize)

    # Estimate positions for each pixel
    ucsx <- mydata$header$ucs_ul_x+(mydata$header$ucs_dx*idmx)
    ucsy <- mydata$header$ucs_ul_y-(mydata$header$ucs_dy*idmy)
    gp <- ucs2latlon(ucsy,ucsx)

    df <- data.frame(lat=gp$lat,lon=gp$lon,x=ucsx,y=ucsy,flux)
    colnames(df)[5] <- mydata$header$description

    return(df)
}
