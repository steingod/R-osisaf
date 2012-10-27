\name{readdailyval}

\alias{readdailyval}

\title{
    Read daily validation data for surface irradiance estimates
}

\description{
    This function is a wrapper for the \link{read.table} function
    specifying variable names for the column oriented validation format
    used for the validation of OSISAF radiative fluxes. This specific
    version is dedicated to validation of estimates against daily
    measurements using pyrgeometers and pyranometers (not in the same
    file, but using the same format for both fluxes).

    Observations are named \code{OBS} and estimates \code{EST}, station id
    is \code{StId}. No specific reference to whether the object contains
    SSI or DLI is currently added. The user has to keep track of this
    himself.

    Missing values are coded.
}

\usage{
    readdailyval(file)
}

\arguments{
    \item{file}{Filename to read, including full path}
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
    t <- readdailyval("/disk1/myfile.txt")
}

\keyword{
}
