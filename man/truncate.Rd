\name{truncate}
\alias{truncate}
\title{Truncate a Binary Stream}
\description{
  Truncates a binary stream at its current position.
}
\usage{
  truncate(stream)
}
\arguments{
  \item{stream}{a stream previously opened in write mode.}
}
\value{
  The new size of the stream.
}
\details{
  This function will truncate the file at the current position.  Any data
  after this position will be deleted.  
}
\examples{
\dontrun{s <- openstream("mydata", "write")
# Write the bytes from 1 to 10 to the file, and truncate after them
# to be sure it is only 10 bytes long.
writeint(s, 1:10, 1)
truncate(s)
closestream(s)}
}
\keyword{file}
