
\name{gammaPDF}
\alias{gammaPDF}

\title{Three parameter Gamma distribution}

\description{
To estimate the probability denisty function of a three parameter 
gamma distribution. The formula is defined in chapter 7.1 of 
Continous Univariate Distributions, Norman L. Johnson, Samuel Kotz, 
N. Balakrishnan, Wiley, ISBN 0-471-58495-9. 
}
\usage{
  gammaPDF(params,x)
}

\arguments{
    \item{params}{vector containing three gamma parameters (alpha, beta,
    gamma, see \link{gammaPDFplot} for details)}
    \item{x}{input values to estimate the gamma value for}
}
\value{\link{vector} containing the corresponding Gamma distribution
values to input vector x}

\seealso{
   \link{gammaPDFplot}, \link{gammaPDF2parest}, \link{gammaPDF3parest},
   \link{GammaDist} 
}

\author{
    Øystein Godøy \email{o.godoy@met.no}
}

\references{
Johnson, N. L., Kotz, S., and Balakrishnan, N., 
\cite{Continous Univariate Distributions}, 
ISBN 0-471-58495-9, Wiley
}

\examples{
}

\keyword{}
