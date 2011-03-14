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
}

\usage{
    readhourlyssival(file)
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
    t <- readhourlyssival("/disk1/myfile.txt")
}

\keyword{
}
