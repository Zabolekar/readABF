\name{plot.ABF}
\alias{plot.ABF}
\title{Plot the Result of readABF}
\description{
  Plot the result of \code{readABF}.
}
\usage{
\method{plot}{ABF}(x, \dots, conductance = FALSE)
}
\arguments{
  \item{x}{the result of \code{readABF}.}
  \item{...}{further arguments to \code{plot.data.frame}}
  \item{conductance}{if \code{TRUE}, make an attempt to calculate and plot the conductance against time. The default behaviour is to plot the first channel of the first sweep, whatever it contains, agains time}
}
\examples{
\dontrun{
# load some data and plot it
r <- readABF("some_data.abf")
plot(r)
}
}
\keyword{file}