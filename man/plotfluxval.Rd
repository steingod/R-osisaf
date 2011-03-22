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
    plotfluxval(x)
}

\arguments{
    \item{x}{a validation object of the type returned by
    \link{readhourlyssival}}
}

\value{
    Returns a \link{data.frame} containing monthly validation data for the
    individual staions used.
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
