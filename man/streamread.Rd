\name{Stream read functions}
\alias{readint}
\alias{readfloat}
\alias{readchar}
\alias{readcomplex}
\alias{readlines}
\title{Read Binary Data from a Stream}
\description{
  Reads binary data from the current position within a stream.
}
\usage{
readint(stream, n = 1, size = 4, signed = TRUE, swapbytes = FALSE)
readfloat(stream, n = 1, size = 8, swapbytes = FALSE)
readcomplex(stream, n = 1, size = 8, swapbytes = FALSE)
readchar(stream, n = 1, len = NA, bufsize = 256)
readlines(stream, n = 1, bufsize = 256,
          eol = getstreameol(stream, bufsize))
}
\arguments{
  \item{stream}{a previously opened stream.}
  \item{n}{the number of items to read.}
  \item{size}{the size of each item, in bytes.}
  \item{signed}{whether to interpret integer data as signed or unsigned.}
  \item{swapbytes}{whether to change little- to big-endian or vice versa.}
  \item{len}{the number of characters to read into each string.}
  \item{bufsize}{the initial buffer size to be used when reading variable
    length strings.  This will be grown as needed.}
  \item{eol}{the end of line marker}
}
\value{
  A vector of integers, doubles, complex values or character strings.
  This may be shorter than requested if there was insufficient data on
  the file.
}
\details{
  Signed integers of sizes 1, 2, 4, and 8 bytes can be read.  Unsigned
  integers of any size up to 8 bytes can be read. (Integers larger than
  are supported in R will be returned in a vector of doubles.)

  Floats of sizes \code{float}, \code{double} and \code{long double}
  can be read (where \code{long double} might be 10, 12 or 16 bytes or
  even the same type as \code{double}).  Complex values using
  any of the float sizes for the real and complex parts can be read.  Any
  size of character string that you can create can be read.

  If \code{swapbytes = TRUE}, then when reading any multi-byte numbers, the
  byte order within the number will be swapped.  This allows you to read
  data that was written on a machine that uses a different convention for
  numeric storage, i.e. read little-endian data from a big-endian machine,
  or vice versa.

  \code{readchar} can either read strings of fixed size given by
  \code{len}, or if \code{len = NA}, ASCII-zero-delimited strings.

  \code{readlines} can read variable length strings with arbitrary
  end of line markers.  See \code{\link{getstreameol}} for the default
  choice of end of line marker.
}
\seealso{
  \code{\link{openstream}}, \code{\link{writeint}},
  \code{\link{getstreameol}}
}
\examples{
## find out endian-ness
if(R.version$major == 1 && R.version$minor >= 2.0) .Platform$endian

s <- openstream("mydata", "write")
writeint(s, 1:10, 1)
writeint(s, 21:30, 2)
writeint(s, 41:50, 4)
writefloat(s, 401:410, 4)
writefloat(s, 801:810, 8)
writechar(s, c("a string", "or two"), asciiz = TRUE)
closestream(s)

s <- openstream("mydata", "read")
readint(s, 10, 1)
readint(s, 10, 2)
readint(s, 10, 4)
readfloat(s, 10, 4)
readfloat(s, 10, 8)
readchar(s, 2)
closestream(s)

dump("openstream", "mydata")
s <- openstream("mydata")
readlines(s, 2)
closestream(s)

unlink("mydata")
}
\keyword{file}
