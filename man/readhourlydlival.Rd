\name{readhourlydlival}

\alias{readhourlydlival}

\title{
    Read hourly DLI validation data
}

\description{
    This function is a wrapper for the \link{read.table} function
    specifying variable names for the column oriented validation format
    used for the validation of OSISAF radiative fluxes. This specific
    version is dedicated to validation of estimates against hourly
    measurements using pyrgeometers.
}

\usage{
    readhourlydlival(file)
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
    t <- readhourlydlival("/disk1/myfile.txt")
}

\keyword{
}
