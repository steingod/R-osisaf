\name{ credf }

\alias{ credf }

\title{
    credf
}

\description{
    This function converts the object returned by \link{readosisaf} to a
    \link{data.frame} that can be used in \link{ggplot2} plot methods.
    Data are unpacked and the header information is used to create
    position information using UCS (Northings and Eastings) as provided by
    the \link{proj4} package. If geographical positions are required use
    \link{ucs2latlon} and append columns to the \link{data.frame}.
    }

\usage{
    credf(mydata,layer=1)
}

\arguments{
    \item{mydata}{A data object as returned by \link{readosisaf}}
    \item{layer}{The layer of the data object to convert}
}

\value{
    \link{data.frame} containing x (Eastings in km), y (Northings in km)
    and the actual product. Origo for the UCS is at the North Pole.
}

\note {
    Input data are currently not well defined and updates of this function
    is to be expected.
}

\seealso{
    \link{readosisaf}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    tmp <- readosisaf("osisaf-prod.hdf5")
    tmp2 <- credf(tmp)
    ggplot()+geom_tile(data=tmp2,aes(x=x,y=y,fill=SSI)+geom_path(data=northatlantic,aes(x=x,y=y,group=group)+coord_equal(xlim=range(tmp2$x),ylim=range(tmp2$y))+scale_fill_gradientn(colours=mycolours(256))
}

\keyword{
}
