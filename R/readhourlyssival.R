#
# NAME:
# readhourlyssival
#
# PURPOSE:
# To generate a data frame containing validation data for hourly SSI
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
# $Id: readhourlyssival.R,v 1.3 2011-04-07 09:49:58 steingod Exp $
#
readhourlyssival <- function(file,format="bioforsk") {

    if (format == "bioforsk") {
        mydata <- read.table(file,
            col.names=
            c("T.sat","EST","NVAL","N","SAT","SOZ","SAZ","RAZ","CM",
                "T.obs","StId","TTM","OBS","ST"),
            na.strings="-999.00")
    } else {
        mydata <- read.table(file,
            col.names=
            c("T.sat","EST","NVAL","N","SAT","SOZ","SAZ","RAZ","CM",
                "T.obs","StId","OBS"),
            na.strings="-999.00")
    }
    return(mydata)
}
