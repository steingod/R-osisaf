
\name{gammaPDFestSAGOB}
\alias{gammaPDFestSAGOB}

\title{Estimate Gamma distribution parameters for SAGOB data}

\description{
Examine a SAGOB dataset and estimate the three parameters of a three
parameter Gamma distribution (or the paramaters of a Normal distribution),
echo these to screen and plot histograms of the data as well as the
selected probability density functions overlayed.
}
\usage{
    gammaPDFestSAGOB(file,feature="CH2",ylim=c(0,1),method="F",resolution=1)
}

\arguments{
    \item{file}{SAGOB file to read}
    \item{feature}{feature to examine}
    \item{ylim}{plot limits of PDF estimates}
    \item{method}{F means \link{gammaPDF3parest}, G means
    \link{gammaPDF2parest}, N means normalPDFest, A means manual
    guessing of all parameters}
    \item{resolution}{resolution of the estimated vector}
}
\value{String message}

\seealso{\link{readSAGOB}, \link{gammaPDFestSAGOBmulti}, \link{gammaPDF}, normalPDFest}

\author{
    Øystein Godøy \email{o.godoy@met.no}
}

\examples{}

\keyword{}
