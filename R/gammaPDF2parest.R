
gammaPDF2parest <- function(dataset="NA") {

    gamma <- as.numeric(readline("Please guess a value for gamma: "))
    print(gamma)
    n <- length(dataset)

    beta <- sum((dataset-gamma)/n,na.rm=T)-(n/sum((dataset-gamma)^-1,na.rm=T))
    alpha <- sum((dataset-gamma)/(n*beta),na.rm=T)

    return(c(alpha,beta,gamma))
}
