\name{osisaf-package}
\alias{osisaf-package}
\alias{osisaf}
\docType{package}
\title{
Useful functions for handling EUMETSAT Ocean and Sea Ice SAF
data
}
\description{
The EUMETSAT OSISAF High Latitude processing facility
provides satellite derived products for sea ice, SST and
radiative fluxes. This package provides an R-interface for
accessing the products in NCSA HDF5 format and basic
statistics, interpretation of these as well as some development tools
utilised by the development team.
}
\details{
\tabular{ll}{
Package: \tab osisaf\cr
Version: \tab 0.1\cr
Date: \tab 2006-04-26\cr
Depends: \tab \cr
Imports: \tab \cr
Suggests: \tab \cr
License: \tab GPL version 2 or newer\cr
URL: \tab http://www.r-project.org, http://saf.met.no\cr
Built: \tab R 2.3.1; i686-redhat-linux-gnu; 2006-09-29 21:57:27; unix\cr
}

Index:
\preformatted{
compHLLMLflux           Compare HL and LML radiative fluxes
compprocchains          Compare EUMETSAT OSISAF products from two
                        different processing chains
latlon2ucs              convert between geographical and UCS
                        coordinates
mondailyssi             Analyse and plot daily EUMETSAT OSISAF SSI
                        validation data
monhourlyssi            Analyse and plot hourly EUMETSAT OSISAF SSI
                        validation data
osisaf-package          Useful functions for handling EUMETSAT Ocean
                        and Sea Ice SAF data ~~ package title ~~
osisafimage             Create image of EUMETSAT OSISAF products
osisafmapdata           GSHHS Low Resolution Coastlines
readosisaf              Read EUMETSAT OSISAF products
ucs2latlon              convert between UCS and geographical
                        coordinates
}
}
\author{
Øystein Godøy <o.godoy@met.no>

Maintainer: Øystein Godøy <o.godoy@met.no>
}
\references{
\url{http://saf.met.no}, \url{http://www.osi-saf.org/}
}
