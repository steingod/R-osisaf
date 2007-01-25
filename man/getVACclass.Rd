
\name{getVACclass}
\alias{getVACclass}

\title{Extract data for a specific class}

\description{
    The data corresponding to a specific VAC class are extracted and
    returned as a new dataset of the same type.
}
\usage{
    getVACclass(x,classname)
}

\arguments{
    \item{x}{Object to subsample}
    \item{classname}{Name of the class wanted, see \link{readVAC} for
    supported classnames. If multiple classes are requested specify them
    as a vector of strings.}
}

\value{returns the same type of object as \link{readVAC}.}

\seealso{
    \link{readVAC}, \link{subset}
}

\author{
    Øystein Godøy \email{o.godoy@met.no}
}

\examples{
    x <- readVAC(filename="/disk2/collocate/VAC-osisaf-2003.txt")
    t <- getVACclass(x,c("CLOUD","ICE","SEA"))
}

\keyword{}
