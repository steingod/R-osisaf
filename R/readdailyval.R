#
# NAME:
# readdailyssival
#
# PURPOSE:
# To generate a data frame containing validation data for daily SSI
# validation.
#
# REQUIREMENTS:
# NA
#
# INPUT:
# NA
#
# OUTPUT:
# NA
#
# NOTES:
# NA
#
# BUGS:
# NA
#
# AUTHOR:
# Øystein Godøy, METNO/FOU, 2011-03-11 
#
# MODIFIED:
# Øystein Godøy, METNO/FOU, 2012-04-13: Fixed handling of missing values.
#
# CVS_ID:
# $Id: readdailyval.R,v 1.2 2012-04-13 22:36:12 steingod Exp $
#
readdailyval <- function(file) {

    mydata <- read.table(file,
            col.names=
            c("T.sat","EST","NVAL","StId","OBS"),
            na.strings="-999.99")
    return(mydata)
}
