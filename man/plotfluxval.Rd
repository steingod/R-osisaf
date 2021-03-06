\name{plotfluxval}

\alias{plotfluxval}

\title{
    Plot flux validation
}

\description{
    Inline with the methodology used by Meteo-France to illustrate
    validation results, this function plots similar diagrams.
}

\usage{
    plotfluxval(x,parameter="bias",stations=NULL,...)
}

\arguments{
    \item{x}{a validation object of the type returned by
    \link{readhourlyssival}}
    \item{parameter}{valid values are "bias", "meanobs", "relbias",
    "biassdev", "relbiassdev" and "nobs"}
    \item{stations}{\link{regexp} string containing station number to use,
    e.g. "11500|44300|56420"}
    \item{...}{additional graphical parameters}
}

\value{
    Returns a \link{list} containing monthly validation data for the
    individual stations used as well as the names of the stations.
}

\seealso{
    \link{readhourlyssival}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    x <- readhourlyssival("myfile.txt")
    tmp <- plotfluxval(x)
}

\keyword{
}
