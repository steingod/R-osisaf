
\name{gammaPDFplot}
\alias{gammaPDFplot}

\title{Plotting of three parameter Gamma distribution}

\description{
A graphical presentation of the Gamma distribution is generated on the
graphical device. This function is added for exemplification of the Gamma
distribution as this have been evaluated for classification of sea ice
from AVHRR data.
}
\usage{
gammaPDFplot(alpha=1,beta=1,gamma=0,xl=c(0,10),yl=c(0,1),res=0.1) 
}

\arguments{
    \item{alpha}{shape factor}
    \item{beta}{scale factor}
    \item{gamma}{location factor}
    \item{xl}{list of minimum and maximum input values}
    \item{yl}{list of minimum and maximum output values to be plotted}
    \item{res}{resolution of the estimated vector with Gamma values}
}
\value{NULL}

\seealso{
    \link{gammaPDF}, \link{gammaPDFtableplot}, \link{GammaDist}
}

\author{
    Øystein Godøy \email{o.godoy@met.no}
}

\examples{}

\keyword{}
