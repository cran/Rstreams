\name{S-PLUS read functions}
\alias{data.restore}
\alias{readSfile}
\alias{SModeNames}
\title{Read an S-PLUS Binary File}
\description{
  Reads binary data files or \code{data.dump} files 
  that were produced in S-PLUS.
}
\usage{
  data.restore(filename, print = FALSE, verbose = FALSE, env = .GlobalEnv)
  readSfile(filename, swapbytes = FALSE)

  SModeNames
}
\arguments{
  \item{filename}{the filename of the S-PLUS \code{data.dump} or binary file.}
  \item{swapbytes}{whether to change big to little-endian or vice versa.}
  \item{print}{whether to print the name of each object as read from the file.}
  \item{verbose}{whether to print the name of every subitem within each object.}
  \item{env}{environment within which to create the restored object.}
}
\value{
  an R version of the S-PLUS object.
}
\details{
  These functions were produced to test and demonstrate the stream
  functions in \code{Rstreams}. \code{readSfile} can read the binary
  files produced in recent versions of S-PLUS on either Windows
  (versions 3.x, 4.x, 2000) or Unix (version 3.x with 4 byte integers).
  It automatically detects whether the file was produced on a big- or
  little-endian machine and adapts itself accordingly.  \code{data.restore}
  can read a similar range of files produced by \code{data.dump}.

  Not all S-PLUS objects can be handled in the current version.  The most
  frequently encountered exceptions are functions and expressions; you'll 
  also have trouble with objects that contain model formulas.  In 
  particular, comments will be lost from function bodies, and the 
  argument lists of functions will often be changed.

  The \code{SModeNames} variable contains a list of the modes of variables
  as used in S-PLUS.
}
\author{
  Duncan Murdoch
}
\seealso{
  \code{\link{openstream}}, \code{\link{data.restore}}.
}
\examples{
pkgdir <- searchpaths()[match("package:Rstreams", search())]
readSfile(file.path(pkgdir, "example", "_Data", "myobj"))
data.restore(file.path(pkgdir, "example", "dumpdata"), print = TRUE)
myobj
}
\keyword{file}
