\name{readABF}
\alias{readABF}
\title{Read an ABF File}
\description{
  Read an ABF file into an ABF object.
}
\usage{
readABF(filename)
}
\arguments{
  \item{filename}{a character string giving the name of the file to load.}
}
\seealso{
  \code{\link{as.data.frame.ABF}}, \code{\link{plot.ABF}}, \code{\link{print.ABF}}
}
\examples{
\dontrun{
## load some data and check the version number
r <- readABF("some_data.abf")
print(r)
}
}
