readVAC <- function(filename) {

    mydata <- read.table(filename,col.names=c("index","MC",
    "SC","sat",
    "lon","lat",
    "year","month","day","hour","minute",
    "A1","A2","T3","T4","T5","A3",
    "SAZ","SOZ","RAZ",
    "elev","surface"),
    na.strings="-99.99")

    mytime <-
    ISOdatetime(mydata$year,mydata$month,mydata$day,mydata$hour,mydata$minute,0)

    return(data.frame(MC=mydata$MC,SC=mydata$SC,sat=mydata$sat,lon=mydata$lon,
    lat=mydata$lat,year=mydata$year,month=mydata$month,day=mydata$day,
    hour=mydata$hour,minute=mydata$minute,time=mytime,
    A1=mydata$A1,A2=mydata$A2,A3=mydata$A3,T3=mydata$T3,T4=mydata$T4,
    T5=mydata$T5,SAZ=mydata$SAZ,SOZ=mydata$SOZ,RAZ=mydata$RAZ,
    elev=mydata$elev,surface=mydata$surface))
}
