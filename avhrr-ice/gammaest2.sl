#
# PROGRAMMING LANGUAGE:
# S-language (for use in R)
#
# FUNCTION NAME:
# gammaest2.sl
#
# PURPOSE:
# To estimate two parameters of a Gamma distribution, by guessing the
# scale parameter using the method defined in chapter 7.1 of Continous 
# Univariate Distributions, Norman L. Johnson, Samuel Kotz, N. Balakrishnan, 
# Wiley, ISBN 0-471-58495-9. Mariken Homleid has a copy of this book.
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

    gamma <- as.numeric(readline("Please guess a value for gamma: "))
    print(gamma)
    n <- length(dataset)

    beta <- sum((dataset-gamma)/n,na.rm=T)-(n/sum((dataset-gamma)^-1,na.rm=T))
    alpha <- sum((dataset-gamma)/(n*beta),na.rm=T)

    return(c(alpha,beta,gamma))
}
