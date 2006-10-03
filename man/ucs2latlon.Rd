
\name{ucs2latlon}
\alias{ucs2latlon}

\title{Convert between UCS and geographical coordinates}

\description{
Coordinates within the Cartesian User Coordinate System, northings and
eastings are converted to geographical latitude and longitude using a
spherical Earth and an Earth radius of 6371. km. This is a very crude
approximation, for better accuracy and support of other Earth models use
e.g. PROJ.4 software (\url{http://www.remotesensing.org/proj}).
}
\usage{
ucs2latlon(northings, eastings)  
}

\arguments{
    \item{northings}{distance to the line through the North Pole and -180
    - 180 degrees longitude in kilometers, negative southwards in the Atlantic}
    \item{eastings}{distance from the Greenwich meridian in kilometers,
    positive eastwards}
}
\value{Returns a \link{data.frame} with latitude (lat) and longitude (lon)
and metadata.
}

\seealso{\link{latlon2ucs}}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}
\note{
}

\examples{
    geopos <- ucs2latlon(northings,eastings)
    geopos$lat
    geopos$lon
}

\keyword{}
