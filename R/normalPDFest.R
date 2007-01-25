#
# PROGRAMMING LANGUAGE:
# S-language (for use in R)
#
# FUNCTION NAME:
# normalpdf.sl
#
# PURPOSE:
# To compute probability density function for a Normal distribution.
#
# NOTES:
# NA
#
# BUGS:
# NA
#
# AUTHOR:
# Øystein Godøy, DNMI/FOU, 08/11/2001
#
normalPDFest <- function(dataset="NA") {

    mu <- mean(dataset,na.rm=T)
    sigma <- sd(dataset,na.rm=T)

    pd <- (1/(sqrt(2*pi)*sigma))*(exp(-1*((dataset-mu)^2/(2*sigma^2))))

    return(list(mean=mu,sdev=sigma,prob=pd))
}
