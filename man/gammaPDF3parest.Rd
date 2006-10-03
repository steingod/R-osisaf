
\name{gammaPDF3parest}
\alias{gammaPDF3parest}

\title{Estimate the three parameters of the Gamma distribution for a
dataset}

\description{
Following the description given in Chapter 7.1 within the book referenced
below the three parameters of a three parameter Gamma distribution is
estimated.
}
\usage{
gammaPDF3parest(dataset)
}

\arguments{
    \item{dataset}{a \link{vector} containing the dataset to estimate
    parameters for}
}
\value{A \link{vector} containing alpha, beta and gamma (the shaope, scale
and location factors).}

\seealso{\link{gammaPDF2parest}, \link{gammaPDFplot}, \link{GammaDist}}

\author{
    Øystein Godøy \email{o.godoy@met.no}
}
\references{
Johnson, N. L., Kotz, S., and Balakrishnan, N., 
\cite{Continous Univariate Distributions},
ISBN 0-471-58495-9, Wiley
}

\examples{}

\keyword{}
