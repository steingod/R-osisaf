
\name{readSAGOB}
\alias{readSAGOB}

\title{Read ASCII files containing satellite for specific situations
observations}

\description{
    Read ASCII files containing satellite data for specific situations.
    These files have usually been created for specific classes. The files
    have a five records long header specifying the contents of the file.
    Each file contain satellite data for a spcific situation (e.g. cloud
    free water, clouds, snow, etc). Visible channels are corrected for the
    solar zenith angle.

    A sample of the files read are given below:
    \preformatted{
 [Storage class] WATER
 [Station and time identification] * *.*.* *:* UTC
 [Number of requested parameters] 9
 [Parameternames] SuZ SaZ SuA SaA CH1 CH2 CH3 CH4 CH5

 73.70 60.93 190.62 31.38 14.78 13.77 291.34 263.17 261.14
 73.70 60.93 190.62 31.38 15.81 14.76 292.56 263.06 260.82
 73.70 60.93 190.62 31.38 17.03 16.08 295.15 263.29 260.84
 73.70 60.93 190.62 31.38 18.71 18.04 294.93 260.69 258.26
 73.70 60.93 190.62 31.38 19.10 18.62 296.43 261.21 258.31
 73.70 60.93 190.62 31.38 21.46 21.09 298.54 259.72 256.88
 73.70 60.93 190.62 31.38 24.37 24.46 301.27 257.83 255.09
 73.70 60.93 190.62 31.38 29.97 30.61 305.12 256.10 253.43
 73.70 60.93 190.62 31.38 30.36 31.00 303.18 254.43 252.13
 73.70 60.93 190.62 31.38 13.38 12.05 290.69 266.29 264.05
 73.70 60.93 190.62 31.38 13.80 12.33 293.77 266.89 264.36
 73.70 60.93 190.62 31.38 17.54 16.71 293.98 263.46 260.76
    }
    In addition several additional paramaters are estimated by default.
}
\usage{
    readSAGOB(filename) 
}

\arguments{
    \item{filename}{full path and filename to read}
}
\value{\link{list} containing two elements \bold{class} and \bold{data}.
\bold{class} contains a string with class description from the file
header, while \bold{data} contains a \link{matrix} with column names
\code{SuZ, SaZ, SuA, SaA, CH1, CH2, CH3, CH4, CH5, CH2/CH1, CH3-CH4,
CH4-CH5, RaZ}. the column names are read from the header, but these are
the most common used.
}

\note{
    The ASCII collocation files are not much used anymore, but the
    function is kept for compability and access to old files.
}
\seealso{}

\author{
    Øystein Godøy \email{o.godoy@met.no}
}

\examples{}

\keyword{}
