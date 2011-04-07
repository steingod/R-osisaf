\name{readhourlyssival}

\alias{readhourlyssival}

\title{
    Read hourly SSI validation data
}

\description{
    This function is a wrapper for the \link{read.table} function
    specifying variable names for the column oriented validation format
    used for the validation of OSISAF radiative fluxes. This specific
    version is dedicated to validation of estimates against hourly
    measurements using pyranometers.

    The format of the input validation data may differ according to the
    underlying observational data as some networks have additional
    information to the irradiance measurements. E.g. the Bioforsk stations
    include observations of the number of minutes with direct insolation
    at the station during the last hour.
}

\usage{
    readhourlyssival(file, format="bioforsk")
}

\arguments{
    \item{file}{Filename to read, including full path}
    \item{format}{Valid formats are \code{bioforsk} and \code{compact}}
}

\value{
    \link{data.frame} as returned by \link{read.table}
}

\seealso{
    \link{monhourlyssi}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    t <- readhourlyssival("/disk1/myfile.txt")
}

\keyword{
}
