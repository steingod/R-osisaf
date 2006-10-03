
gammapdf <- function(params,x) {
    pd<-((((x-params[3])^(params[1]-1)*(exp((-(x-params[3]))/(params[2])))))/((params[2]^params[1])*gamma(params[1])))

    return(pd)
}
