#
# PROGRAMMING LANGUAGE:
# S-language (for use in R)
#
# FUNCTION NAME:
# gammaest.sl
#
# PURPOSE:
# To estimate the three parameters of a Gamma distribution using the
# method defined in chapter 7.1 of Continous Univariate Distributions,
# Norman L. Johnson, Samuel Kotz, N. Balakrishnan, Wiley, ISBN
# 0-471-58495-9. Mariken Homleid has a copy of this book.
#
# NOTES:
# NA
#
# BUGS:
# NA
#
# AUTHOR:
# Øystein Godøy, DNMI/FOU, 13/06/2001
#
function(dataset="NA") {
    m1 <- mean(dataset,na.rm=T)
    m2 <- (sum((dataset-m1)^2,na.rm=T))/(length(dataset))
    m3 <- (sum((dataset-m1)^3,na.rm=T))/(length(dataset))

    alpha <- ((4*(m2^3))/(m3^2))
    beta <- ((0.5*m3)/(m2))
    gamma <- (m1-((2*(m2^2))/(m3)))

    return(c(alpha,beta,gamma))
}
