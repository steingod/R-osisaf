
\name{readosisaf}
\alias{readosisaf}

\title{Read EUMETSAT OSISAF products}

\description{
EUMETSAT Ocean and Sea Ice Satellite Application Facility (OSISAF)
(\url{http://www.osi-saf.org}) products in NCSA HDF5 format from the High
Latitude processing center (\url{http://saf.met.no}) are accessed using
the library used to generate the products (libosihdf5).
}
\usage{
readosisaf(filename,layers=1,nomiss=TRUE)  
}

\arguments{
    \item{filename}{file to access}
    \item{layers}{The number of layers to read out. Defaults to 1 as some
    products mix datatypes within the layer group}
    \item{nomiss}{missing values are replaced by R NAs}
}
\value{Returns a \link{list} with data and metadata. Character strings are
yet not returned.
\item{header}{A list containing metadata. the following metadata is
provided: filename (name of the file read), description (short textual
description of the main file content), year (valid year for the data),
month (valid month for the data), day (valid day of month for the data),
hour (valid hour), minute (valid minute), xsize (number of columns in
original file as stored in HDF5), ysize (number of rows in the opriginal
file as stored in HDF5), zsize (number of levels in the datafile, only the
first level is returned), and package (name of package that created the
dataset). The time specification is by default UTC. }
\item{data}{A vector containing the first data field within the HDF5 file}
}

\seealso{\link{osisafimage}}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}
\note{
    All product groups of the OSISAF HL centre (various sea ice variables,
    SST and radiative fluxes (downward shortwave and longwave)) is
    supported. However, the HDF5 files generated may contain several
    layers as indicated by the variable zsize in the header of the files.
    The current version of the osisaf R-package will only return the first
    layer, i.e. the product itself. The current version does not return
    quality flags and any auxiliary data that might be appended to the
    file for some reason.
}

\examples{
    # Read a sea ice concentration field
    t <- readosisaf(file="/data/osisaf/output/ice/multi/conc/ice_conc_nh_200609181200.hdf")
}

\keyword{}
