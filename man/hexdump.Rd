\name{hexdump}
\alias{hexdump}
\title{Display a Hex Dump of a Stream}
\description{
  Displays a mixed hex and ascii dump of the content of a stream.
}
\usage{
  hexdump(s, len)
}
\arguments{
  \item{s}{an already opened stream.}
  \item{len}{optional: how many bytes to read and display.}
}
\value{none}
\details{
  Reads bytes from the stream and displays them on the standard
  output location, in a mixed hex and ascii display.

  The default is to dump the rest of the stream; if a shorter length
  is given, only that many bytes will be read and displayed.
}
\seealso{
  \code{\link{openstream}}
}
\examples{
s <- openstream("mydata", "write")
writeint(s, 1:100, 1)
closestream(s)
s <- openstream("mydata", "read")
hexdump(s)
seekstream(s,0)
hexdump(s,10)
closestream(s)
unlink("mydata")
}
\keyword{file}
