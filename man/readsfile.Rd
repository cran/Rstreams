\name{readSfile}
\alias{readSfile}
\alias{ReadSObj}
\alias{SModeNames}
\title{Read an S-PLUS Binary File}
\description{
  Reads simple binary data files that were produced in S-PLUS.
}
\usage{
  readsfile(filename, swapbytes = FALSE)
}
\arguments{
  \item{filename}{the filename of the S-PLUS binary file.}
  \item{swapbytes}{whether to change big to little-endian or vice versa.}
}
\value{
  an R version of the S-PLUS object.
}
\details{
  This function was produced to test and demonstrate the stream
  functions in \code{Rstreams}. It can read the binary files produced in
  recent versions of S-PLUS on either PCs ((versions 3 to 2000 on
  DOS/Windows) or Unix (version 3.x with 4 byte integers). It
  automatically detects whether the file was produced on a big- or
  little-endian machine and adapts itself accordingly.

  Not all S-PLUS objects can be handled in the current version.  The most
  frequently encountered exceptions are functions and expressions; you'll 
  also have trouble with objects that contain model formulas.

  \code{ReadSObj} is used internally by readsfile.  Look at the
  \code{ReadSObj} source code if you're interested in the S-PLUS object
  format.
}
\examples{
pkgdir <- searchpaths()[match("package:Rstreams", search())]
readSfile(file.path(pkgdir, "R-ex", "_Data", "myobj"))
}
\keyword{file}
