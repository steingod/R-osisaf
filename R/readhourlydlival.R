#
# NAME:
# readhourlydlival
#
# PURPOSE:
# To generate a data frame containing validation data for hourly DLI
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
# $Id: readhourlydlival.R,v 1.1 2011-04-07 09:49:58 steingod Exp $
#
readhourlydlival <- function(file) {

    mydata <- read.table(file,
            col.names=
            c("T.sat","EST","NVAL","N","SAT","SOZ","SAZ","RAZ","CM",
                "T.obs","StId","OBS"),
            na.strings="-999.00")
    return(mydata)
}
