getVACclass <- function(x,classname){

    t <- subset(x,x$MC==classname)

    return(t)
}
