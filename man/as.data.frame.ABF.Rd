\name{as.data.frame.ABF}
\alias{as.data.frame.ABF}
\title{Convert the Result of readABF to a Data Frame}
\description{
  Convert the result of \code{readABF} to a data frame that can be easily plotted.
}
\usage{
\method{as.data.frame}{ABF}(x, current = 1, voltage = 2)
}
\arguments{
  \item{x}{the result of \code{readABF}.}
  \item{current}{a number indicating which column of the dataset contains the current.}
  \item{voltage}{a number indicating which column of the dataset contains the voltage.}
}
\examples{
\dontrun{
# load some data and plot it
r <- readABF("some_data.abf")
plot(as.data.frame(r))
}
}
\keyword{file}