\name{Stream write functions}
\alias{writeint}
\alias{writefloat}
\alias{writecomplex}
\alias{writechar}
\title{Write Binary Data to a Stream}
\description{
  Writes binary data to the current position within a stream.
}
\usage{
writeint(stream, data, size = 4, swapbytes = FALSE)
writefloat(stream, data, size = 8, swapbytes = FALSE)
writecomplex(stream, data, size = 8, swapbytes = FALSE)
writechar(stream, data, asciiz = FALSE)
}
\arguments{
  \item{stream}{a stream previously opened in write mode.}
  \item{data}{a vector of data to write.}
  \item{size}{the size of each item, in bytes.}
  \item{swapbytes}{whether to swap the byte order when writing.}
  \item{asciiz}{whether to append an ascii zero to each character 
                vector component.}
}
\value{none}
\details{
  Integers can be written in sizes of 1, 2, 4, and 8 bytes.  

  Numeric values can be written as \code{float}, \code{double} or
  \code{long double}.
  
  Any size of character string that you can create can be written.

  If \code{swapbytes = TRUE}, then when writing any multi-byte numbers, the
  byte order within the number will be swapped.  This allows you to write
  data for a machine that uses a different convention for numeric storage,
  i.e. write little-endian data from a big-endian machine, or vice versa.

  If \code{asciiz = FALSE} (the default), there is no separator between the
  components of a character vector being written.
}
\examples{
sampledata <- 1:100
s <- openstream("mydata", "write")
# Write the bytes from 1 to 100 to the file
writeint(s, sampledata, 1)
closestream(s)
unlink("mydata")
}
\keyword{file}
