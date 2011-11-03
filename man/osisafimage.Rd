
\name{osisafimage}
\alias{osisafimage}

\title{Create image of EUMETSAT OSISAF products}

\description{
EUMETSAT Ocean and Sea Ice Satellite Application Facility (OSISAF
(\url{http://www.osi-saf.org}) products in NCSA HDF5 format from the High
Latitude processing center (\url{http://saf.met.no}) are accessed using
the library used to generate the products. This function reads the output
from readosisaf and creates an image on the graphical device used.
}
\usage{
osisafimage(x, layer=1, map=FALSE)  
}

\arguments{
    \item{x}{R object (\link{list}) returned by \link{readosisaf}}
    \item{layer}{The data layer to visualise (1 or higher)}
    \item{map}{logical telling whether coast lines should be added}
}
\value{None}

\seealso{\link{readosisaf}}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}
\note{
    The current version supports map overlay only for the standard map
    projection of the OSISAF HL centre. That is Polar Stereographic
    correct at 60 degrees North and not rotated.
}

\examples{
    # Read a sea ice concentration field
    t <- readosisaf(file="/opdata/osisaf/output/ice/multi/conc/ice_conc_nh_200609181200.hdf")
    # Generate image on graphical device
    osisafimage(t)
}

\keyword{}
