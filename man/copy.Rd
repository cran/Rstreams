\name{copystream}
\alias{copystream}
\title{Copy from One Stream to Another}
\description{
  Copies bytes from the current position of one stream to the current
  position of another.
}
\usage{
  copystream(src, dest, nbytes)
}
\arguments{
  \item{src}{a stream previously opened in read mode.}
  \item{dest}{a stream previously opened in write mode.}
  \item{nbytes}{how many bytes to copy}
}
\value{
  how many bytes were actually copied.
}
\details{
  If you request copying more bytes than are present in the source, values
  will not be repeated:  only what is there will be copied.
}
\examples{
s <- openstream("mydata", "write")
# Write the bytes from 1 to 100 to the file
writeint(s, 1:100, 1)
closestream(s)
s1 <- openstream("mydata", "read")
s2 <- openstream("newdata", "write")
# Write a header in text, then copy the rest
writechar(s2, "Header")
copystream(s1, s2, summary(s1)$size)
closestream(s1)
closestream(s2)
unlink(c("mydata", "newdata"))
}
\keyword{file}
