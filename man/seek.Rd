\name{seekstream}
\alias{seekstream}
\title{Set Binary Stream to New Position}
\description{
  Sets a binary stream to a new position.
}
\usage{
  seekstream(stream, offset, origin = "start")
}
\arguments{
  \item{stream}{a previously opened stream.}
  \item{offset}{the offset to seek to.}
  \item{origin}{the base to which to apply offset.}
}
\value{
  The new position of the stream.
}
\details{
  The origin may be \code{"start"}, in which case offset is relative to
  the start of the file, \code{"current"}, in which case it is relative to
  the current position, or \code{"end"}, in which case it is relative to
  the end of the file.
}
\examples{
s <- openstream("mydata", "write")
# Write the bytes from 1 to 100 to the file
writeint(s, 1:100, 1)
# Write the bytes from 1 to 10 to the last 10 bytes of the file
seekstream(s, -10, "end")
writeint(s, 1:10, 1)
closestream(s)
s <- openstream("mydata", "read")
readint(s, 100, 1)
closestream(s)
unlink("mydata")
}
\keyword{file}
