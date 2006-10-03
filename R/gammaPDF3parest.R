
gammaPDF3parest <- function(dataset="NA") {
    m1 <- mean(dataset,na.rm=T)
    m2 <- (sum((dataset-m1)^2,na.rm=T))/(length(dataset))
    m3 <- (sum((dataset-m1)^3,na.rm=T))/(length(dataset))

    alpha <- ((4*(m2^3))/(m3^2))
    beta <- ((0.5*m3)/(m2))
    gamma <- (m1-((2*(m2^2))/(m3)))

    return(c(alpha,beta,gamma))
}
