\name{as.data.frame.ABF}
\alias{as.data.frame.ABF}
\title{Convert the Result of readABF to a Data Frame}
\description{
  Convert the result of \code{readABF} to a data frame.
}
\usage{
\method{as.data.frame}{ABF}(x, row.names = NULL, optional = FALSE, ..., sweep = NULL)
}
\arguments{
  \item{x}{the result of \code{readABF}.}
  \item{sweep}{a number indicating which sweep to convert, if there are multiple sweeps.}
  \item{row.names, optional, ...}{further arguments in case they are passed from other methods; will be ignored.}
}
\examples{
\dontrun{
# load some data and plot it
r <- readABF("some_data.abf")
as.data.frame(r)
}
}
\keyword{file}