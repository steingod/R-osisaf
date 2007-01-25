
\name{solzencorrect}
\alias{solzencorrect}

\title{Correct data for solar zenith angle}

\description{
    Data influenced by the solar zenith angle are normalised by dividing
    the values by the cosine of the solar zenith angle.
}

\usage{
    solzencorrect(soz,data)
}

\arguments{
    \item{solzen}{Vector of solar zenith angles (in degrees)}
    \item{data}{Vector of data to be corrected}
}
\value{
    Returns the corrected data in a \link{vector}.
}

\seealso{}

\author{
    Øystein Godøy \email{o.godoy@met.no}
}

\examples{}

\keyword{}
