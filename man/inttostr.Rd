\name{inttostr}
\alias{inttostr}
\title{Converts integers to strings in different bases}
\description{
  Converts an integer to a string representation in base 2 to 36.
}
\usage{
  inttostr(x, digits=0, base=16, complement=FALSE))
}
\arguments{
  \item{x}{a vector of integers to display}
  \item{digits}{the minimum number of digits to use}
  \item{base}{the numeric base of the representation}
  \item{complement}{whether to keep the sign on negatives or
    use a complement display}
}
\value{a character vector giving the representation of each integer}
\details{
All of the integers in x will be converted using the same number of digits.  

If \code{complement=TRUE}, then instead of keeping negative signs, all
negative values will have \code{base^digits added}. In order for this scheme to
produce unique strings, \code{base^digits/2} must be bigger than the largest
number.  If this is not the case, then complements will not be taken.

See the examples.
}
\examples{
# Display using signed notation, with leading zeros
inttostr(c(-1,1), 4)
# Display using complement notation
inttostr(c(-1,1), 4, complement=TRUE)
}
\keyword{file}
