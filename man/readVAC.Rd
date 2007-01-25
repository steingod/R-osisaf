
\name{readVAC}
\alias{readVAC}

\title{Read ASCII files containing satellite for specific situations
observations}

\description{
    Read ASCII files containing satellite data for specific situations.
    These files have usually been created for specific classes.
}
\usage{
    readVAC(filename) 
}

\details{
    VAC = Visible Avhrr Collocation

    \preformatted{

    Output file format:
    -------------------

    The VAC result files are in ASCII format. There are no header
    data in the files. The files are organized in fixed formatted
    coloumns divided by at least one blank.

    There are 22 columns with the following content:

     1: Index. Same number indicates same collocation object
     2: Main class object classification (see README_classes.txt)
     3: Sub class object classification (see README_classes.txt)
     4: Satellite name
     5: Pixel latitude
     6: Pixel longitude
     7: Year
     8: Month
     9: Day
    10: Hour
    11: Minute
    12: Channel 1 albedo
    13: Channel 2 albedo
    14: Channel 3B brightness temperature
    15: Channel 4 brightness temperature
    16: Channel 5 brightness temperature
    17: Channel 3A albedo
    18: Satellite zenith angle
    19: Solar zenith angle
    20: Relative azimuth angle
    21: Elevation of pixel (in meters)
    22: Surface/land use class

    Albedo values are given in 0-100%, brightness temperature in Kelvin.
    Negative channel values indicate undefined values.

    Elevation and surface class are from the USGS data base.
    Elevation is in meters. Surface classes as follows:

    USGS Land Use/Land Cover System Legend (Modified Level 2)
    Value   Code    Description
    1       100     Urban and Built-Up Land
    2       211     Dryland Cropland and Pasture
    3       212     Irrigated Cropland and Pasture
    4       213     Mixed Dryland/Irrigated Cropland and Pasture
    5       280     Cropland/Grassland Mosaic
    6       290     Cropland/Woodland Mosaic
    7       311     Grassland
    8       321     Shrubland
    9       330     Mixed Shrubland/Grassland
    10      332     Savanna
    11      411     Deciduous Broadleaf Forest
    12      412     Deciduous Needleleaf Forest
    13      421     Evergreen Broadleaf Forest
    14      422     Evergreen Needleleaf Forest
    15      430     Mixed Forest
    16      500     Water Bodies
    17      620     Herbaceous Wetland
    18      610     Wooded Wetland
    19      770     Barren or Sparsely Vegetated
    20      820     Herbaceous Tundra
    21      810     Wooded Tundra
    22      850     Mixed Tundra
    23      830     Bare Ground Tundra
    24      900     Snow or Ice
    99              Interrupted Areas (Goodes Homolosine Projection)
    100             Missing Data

    Remarks:
    --------

    The index value is used in the collocation process. These numbers are
    unique for each object in the original collocation files. When passage
    collocation files are concatinated, these values are no longer unique.
    But the index value can still be used to separate different
    collocation objects as two following objects on the files will never
    have equal indexes. This is because there must always be at least two
    objects on a collocation input file.

    Classes:
    --------

    There are 12 main classes that are used in VAC. These are:

    SNOW    -  Cloud free land areas covered by snow.
    UNCSN   -  Uncertain snow, possible snow free land (cloud free).
    LAND    -  Cloud free land areas with NO snow.
    ICE     -  Cloud free sea areas covered with ice and/or snow.
    WETIC   -  Cloud free wet sea ice/glacier ice.
    UNCIC   -  Uncertain sea ice, possible open sea (cloud free).
    SEA     -  Cloud free sea areas with NO snow or ice.
    CLOUD   -  Land or sea areas covered by clouds.
    MIXSL   -  A mixture of cloud free snow and land with no snow.
    MIXMO   -  A mixture of mountain, flat and shadow, most probably with snow.
    MIXSI   -  A mixture of cloud free sea ice, possible with some open sea.
    SHAD    -  Shadow, from cloud over cloud, land or snow.
    }
    For a list of the subclasses available, please refer to the
    documentation provided by Steinar Eastwood \email{s.eastwood@met.no}.
}

\arguments{
    \item{filename}{full path and filename to read}
}
\value{
    readVAC use \link{read.table} to access the external data, thus data
    are returned as a \link{data.frame} using relatively self explaining
    names on the columns according to the description given above.
}

\note{
}

\seealso{\link{plotVAChist}}

\author{
    Øystein Godøy \email{o.godoy@met.no}
}

\references{
    The VAC system is developed and maintained by Steinar Eastwood
    \email{s.eastwood@met.no}. Please contact him for further information
    on the system.
}

\examples{}

\keyword{}
