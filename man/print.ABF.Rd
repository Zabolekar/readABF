\name{print.ABF}
\alias{print.ABF}
\title{Print a Short Representation of the Result of readABF}
\description{
  Print some information about the result of \code{readABF} that helps identify the dataset but doesn't overwhelm the user with raw data.
}
\usage{
\method{print}{ABF}(x, ...)
}
\arguments{
  \item{x}{the result of \code{readABF}.}
  \item{...}{further arguments in case they are passed from other methods; they will be ignored.}
}
\examples{
\dontrun{
# load some data and plot it
r <- readABF("some_data.abf")
print(r)
}
}
\keyword{file}