\name{osisafmapdata}
\docType{data}
\alias{osisafmapdata}

\title{GSHHS Low Resolution Coastlines}

\description{
    GSHHS - A Global Self-consistent, Hierarchical, High-resolution
    Shoreline Database. GSHHS is a high-resolution shoreline data set
    amalgamated from two data bases in the public domain. The data have
    undergone extensive processing and are free of internal
    inconsistencies such as erratic points and crossing segments. The
    shorelines are constructed entirely from hierarchically arranged
    closed polygons. The data can be used to simplify data searches and
    data selections, or to study the statistical characteristics of
    shorelines and land-masses. It comes with access software and routines
    to facilitate decimation based on a standard line-reduction algorithm.

    The dataset provided herein have been extracted using the software
    provided by the National Geophysical Data Center of NOAA Satellite and
    Information Service. The original dataset contains lakes, islands
    within lakes etc. This dataset contain only the coastlines.
}

\usage{
    data(osisafmapdata)
}

\format{
    A \link{data.frame} containing two elements, latitude (lat) and
    longitude (lon) in decimal degrees.
}

\source{
    \url{http://www.ngdc.noaa.gov/mgg/shorelines/gshhs.html}

    GSHHS is developed and maintained by:

	Dr. Paul Wessel, SOEST, University of Hawai'i, Honlulu, HI.
	wessel@soest.hawaii.edu, and

	Dr. Walter H. F. Smith, NOAA Laboratory for Satellite Altimetry,
	National Oceanographic Data Center, Silver Spring, MD.
	Walter.HF.Smith@noaa.gov
}

\references{
    Wessel, P., and W. H. F. Smith, \cite{A Global Self-consistent,
    Hierarchical, High-resolution Shoreline Database}, J. Geophys. Res.,
    101, #B4, pp.  8741-8743, 1996. 
}

\keyword{datasets}
