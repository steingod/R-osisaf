
\name{gammaPDFestSAGOBmulti}
\alias{gammaPDFestSAGOBmulti}

\title{Estimate gamma distribution parameters for various SAGOB features}

\description{
Same as \link{gammaPDFestSAGOB} but for several features. Filenames are
expected to be of the type \code{class_<monthname><year>_<overcast>.txt}
where monthname is a lowercase three letter name, and year a two digit
specification (see \link{monthnameconvert}).
}
\usage{
gammaPDFestSAGOBmulti(month="mar", class="water", overcast="c", method="F")
}

\arguments{
    \item{basepath}{directory to search for files}
    \item{month}{three letter lowercase month name}
    \item{year}{two digit year specification}
    \item{class}{name of the class to read (as specified in filename)}
    \item{overcast}{cloud cover conditions as specified in filename}
    \item{method}{see \link{gammaPDFestSAGOB} for details}
}
\value{None}

\seealso{\link{gammaPDFestSAGOB}, \link{monthnameconvert}}

\author{
    Øystein Godøy \email{o.godoy@met.no}
}

\examples{}

\keyword{}
