
\name{monhourlyssi}
\alias{monhourlyssi}

\title{Analyse and plot hourly EUMETSAT OSISAF SSI validation data}

\description{
    ASCII collocation files containing hourly values of OSISAF 
    estimates and in situ observations of Surface Shortwave Irradiance
    (SSI) are read, basic statistics are generated and the results are
    plotted for validation purposes.
}
\usage{
monhourlyssi(file,method="S",printIt=FALSE,thc=1.1,tho=1.9,sat="a") 
}

\arguments{
    \item{file}{collocation file to read}
    \item{method}{specification of which methods that are available for
    analysis. The methods currently implemented are scatter plot (S),
    scatterplot for clear sky (SC), scatterplot for overcast (SO),
    difference plot as function of vector index (D), difference plot for
    solar zenith angle (DSOZ), difference plot as function of satellite
    zenith angle (DSAZ), difference plot as function of relative azimuth
    angle (DRAZ), boxplot for satellites (DSAT), boxplots for satellites
    for clear sky (DSATC) and overcast sky (DSATO) as determined by NWCSAF
    PPS cloudtypes, and using both NWCSAF PPS and in situ measurements
    of minutes of direct insolation (DSATCC and DSATOO, use thc and
    tho with these), difference plot as function of observed direct
    insolation in minutes (DST), difference plot as function of NWCSAF
    PPS cloudtypes (DC), difference plot as function of hour of the
    day (DH), boxplots for observation sites (POS), for this method
    POSC, POSO, POSCC and POSOO are also available, difference as
    function of time (DTS), DTSC, DTSO, DTSCC, DTSOO are also
    available for this choice and finally the NWCSAF cloudtype can be
    plotted agains the measurement of minutes of direct insolation at
    the station (STCC).}
    \item{thc}{threshold value for clear sky according to NWCSAF PPS
    cloudtype}
    \item{tho}{threshold value for overcast sky according to NWCSAF PPS
    cloudtype}
    \item{printIt}{print to default printer}
}
\value{None}

\seealso{\link{mondailyssi}}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}
\note{
    This function is probably of most interest to the development team
    as the validation files are not distributed in any manner.
}
\examples{}
