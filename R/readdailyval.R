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
# NA
#
# CVS_ID:
# $Id: readdailyval.R,v 1.1 2011-04-07 09:49:58 steingod Exp $
#
readdailyval <- function(file) {

    mydata <- read.table(file,
            col.names=
            c("T.sat","EST","NVAL","StId","OBS"),
            na.strings="-999.00")
    return(mydata)
}
