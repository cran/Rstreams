\name{openstream}
\alias{openstream}
\alias{closestream}
\alias{print.stream}
\alias{summary.stream}
\title{Open or Close a Stream}
\description{
  Open or close a file for binary reading and writing.
}
\usage{
stream <- openstream(filename, mode = "read")
closestream(stream)
}
\arguments{
  \item{filename}{a string containing a single file name. Tilde-expansion
    is performed on the file name.}
  \item{mode}{the mode in which to open the file, either \code{"read"}
    or \code{"write"}.}
  \item{stream}{a previously opened stream, or the special string
    \code{"all"} to close all streams.}
}
\value{
  an object of the stream class is returned by \code{openstream}.
}
\examples{
s <- openstream("mydata", "write")
s
# Write the bytes from 1 to 100 to the file
writeint(s, 1:100, 1)
summary(s)
closestream(s)
\testonly{summary(s)}
unlink("mydata")
}
\keyword{file}
