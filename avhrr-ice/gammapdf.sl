#
# PROGRAMMING LANGUAGE:
# S-language (for use in R)
#
# FUNCTION NAME:
# gammapdf.sl
#
# PURPOSE:
# To estimate the probability denisty function of a three parameter 
# gamma distribution. The formula is defined in chapter 7.1 of 
# Continous Univariate Distributions, Norman L. Johnson, Samuel Kotz, 
# N. Balakrishnan, Wiley, ISBN 0-471-58495-9. Mariken Homleid has a 
# copy of this book.
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
function(params,x) {
    pd<-((((x-params[3])^(params[1]-1)*(exp((-(x-params[3]))/(params[2])))))/((params[2]^params[1])*gamma(params[1])))

    return(pd)
}
