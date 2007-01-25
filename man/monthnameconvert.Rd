
\name{monthnameconvert}
\alias{monthnameconvert}

\title{Convert month specification in filenames}

\description{
Converts month specification in filenames from e.g. sep00 to September
2000.
}
\usage{
    monthnameconvert(name,baseyear=2000)
}

\arguments{
    \item{name}{filename of type \file{myname_sep00.txt}, _ should be used
    in myname}
    \item{baseyear}{as year in the filename is two digits, a baseyear is
    required for proper identification of the year}
}
\value{A string containing full name of month and year separated by a
space character.}

\seealso{}

\author{
    Øystein Godøy \email{o.godoy@met.no}
}

\examples{}

\keyword{}
