\name{plotVAChist}
\alias{plotVAChist}

\title{
    Plot histogram of VAC data for specific classes and features
}

\description{
    Multiple histograms of various features, either single channels or
    channel combinations are plotted using colours. For details on the
    feature combinations supported see details below. For details on the
    classes supported see \link{readVAC}.
}

\usage{
    plotVAChist(vac,classes=c("CLOUD","ICE","SEA"),feature="T4",ploline=F,solarcorrect=T, returnhist=F, ...)
}

\arguments{
    \item{vac}{VAC object as returned by \link{readVAC}}
    \item{classes}{Strings describing the classes to plot}
    \item{feature}{String describing the feature to plot, see details for
    valid features}
    \item{ploline}{Logical toggling histogram style plotting or line
    style}
    \item{solarcorrect}{Logical telleing whether solar influenced features
    should be corrected by \link{solzencorrect}. This is only done for
    features composed of single channels and not for thermal channels.}
    \item{returnhist}{Returns a \link{list} containing the histograms
    requested}
    \item{...}{Arguments to be passed to \link{plot} and other functions.}
}

\details{
    The strings defining the features to present are listed below, with a
    brief introduction:

    \preformatted{
	A1: Reflectance of AVHRR channel 1 (0.6 micron)
	A2: Reflectance of AVHRR channel 2 (0.9 micron)
	A3: Reflectance of AVHRR channel 3A (1.6 micron)
	T3: Brightness temperature of AVHRR channel 3B (3.7 micron)
	T4: Brightness temperature of AVHRR channel 4 (11.5 micron)
	T5: Brightness temperature of AVHRR channel 5 (12.5 micron)
	R21: Ratio of reflectance in AVHRR channels 2 and 1.
	R31: Ratio of reflectance in AVHRR channels 3A and 1.
	D45: Difference in brightness temperatures in AVHRR channels 4 and 5.
	D34: Difference in brightness temperatures in AVHRR channels 3B and 4. 
    }
}

\value{
    If requested by logical option \code{returnhist} a \link{list}
    containing the histogram objects created within the function is
    returned.
}

\seealso{
    \link{readVAC}, \link{solzencorrect}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no}), but recoded from prototype
    developed by Steinar Eastwood (\email{s.eastwood@met.no}).
}

\examples{
}

\keyword{
}
