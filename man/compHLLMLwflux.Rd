
\name{compHLLMLflux}
\alias{compHLLMLflux}

\title{Compare HL and LML radiative fluxes}

\description{
External software reads, interpolates (in time and space) and collocates
radiative fluxes generated at OSISAF High Latitude and Low and Mid
Latitude centres. Results are stored within ASCII files that are read and
analysed by this function. Filenames have standardised names, to access
them a basepath and an experiment identification has to be provided.
Global statistics and illustrations are produced on the active graphical
device. During the process, questions of whether the output should be
written to a file will be asked. The files created have standard names and
are in JPEG format.
}
\usage{
compHLLMLflux(basepath,experiment)  
}

\arguments{
    \item{basepath}{path to collocation files}
    \item{experiment}{experiment identification (string)}
}
\value{None}

\seealso{\link{}}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}
\note{
Not finished
}

\examples{
compHLLMLfluxes(basepath="/data/osisaf/output/fluxes/qc",experiment="200601-200712")
}

\keyword{}
