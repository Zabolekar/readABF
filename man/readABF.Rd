\name{readABF}
\alias{readABF}
\title{Read an ABF File}
\description{
  Read an ABF File.
}
\usage{
readABF(filename)
}
\arguments{
  \item{filename}{a character string giving the name of the file to load.}
}
\seealso{
  \code{\link{data.vs.time}}.
}
\examples{
\dontrun{
## load some data and check the version number
r <- readABF("some_data.abf")
print(r$format_version)
}
}
