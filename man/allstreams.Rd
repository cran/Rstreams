\name{allstreams}
\alias{allstreams}
\title{Get All Open Streams}
\description{
  Gets a vector containing all currently open binary stream files.
}
\usage{
  allstreams()
}
\value{
  a vector of stream handles, of class \code{"stream"}.
}
\details{
  Only currently open streams will be returned.
}
\examples{
# Print a list of all open streams
allstreams()
# Close the second open stream
\dontrun{closestream(allstreams()[2])}
# Close all streams
closestream('all')
}
\keyword{file}
