
\name{mondailyssi}
\alias{mondailyssi}

\title{Analyse and plot daily EUMETSAT OSISAF SSI validation data}

\description{
    ASCII collocation files containing daily mean values of OSISAF
    estimates and in situ observations of Surface Shortwave Irradiance
    (SSI) are read, basic statistics are generated and the results are
    plotted for validation purposes.
}
\usage{
mondailyssi(filename, method="S",printIt=FALSE) 
}

\arguments{
    \item{filename}{collocation file to read}
    \item{method}{specification of which methods that are available for
    analysis. The methods currently implemented are scatter plot (S),
    difference plot by time (D), and boxplots for observation sites (POS).}
    \item{printIt}{print to default printer}
}
\value{None}

\seealso{\link{monhourlyssi}}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}
\note{
    This function is probably of most interest to the development team
    as the validation files are not distributed in any manner.
}
\examples{}
