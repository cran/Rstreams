\name{getstreameol}
\alias{getstreameol}
\title{Determine a Stream's End-of-Line Marker}
\description{
  Examines a stream to determine the end-of-line marker.
}
\usage{
  getstreameol(stream, bufsize = 256)
}
\arguments{
  \item{stream}{an already opened stream.}
  \item{bufsize}{buffer size for use by \code{\link{readchar}}.}
}
\value{A character variable holding the end-of-line marker.}
\details{
  Reads characters from the stream until it finds a CR, LF, or CR/LF
  end of line marker.  These are the defaults used in text files
  on the Mac, Unix, and PCs respectively. Returns marker in a string, as
  \code{'\\r'}, \code{'\\n'}, or \code{'\\r\\n'}.
}
\seealso{
  \code{\link{readlines}},\code{\link{openstream}}
}
\examples{
dump("getstreameol","mydata")
s <- openstream("mydata")
getstreameol(s)
closestream(s)
unlink("mydata")
}
\keyword{file}
