#
# PROGRAMMING LANGUAGE:
# S-language (for use in R)
#
# FUNCTION NAME:
# monthconv.sl
#
# PURPOSE:
# To convert month specification i filenames from e.g. sep00 to September
# 2000.
#
# NOTES:
# NA
#
# BUGS:
# NA
#
# AUTHOR:
# Øystein Godøy, DNMI/FOU, 14/06/2001
#
function(name,baseyear=2000) {

    fname <- strsplit(name,"_")
    mon <- substr(fname[[1]][2],0,3)
    yy <- substring(fname[[1]][2],4)
    yy <- as.numeric(yy)+baseyear

    if (mon=="jan")
	mon <- "January"
    else if (mon=="feb")
	mon <- "February"
    else if (mon=="mar")
	mon <- "March"
    else if (mon=="apr")
	mon <- "April"
    else if (mon=="may")
	mon <- "May"
    else if (mon=="jun")
	mon <- "June"
    else if (mon=="jul")
	mon <- "July"
    else if (mon=="aug")
	mon <- "August"
    else if (mon=="sep")
	mon <- "September"
    else if (mon=="oct")
	mon <- "October"
    else if (mon=="nov")
	mon <- "November"

    datestr <- paste(mon,yy," ")

    return(datestr)
}
