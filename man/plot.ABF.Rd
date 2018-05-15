\name{plot.ABF}
\alias{plot.ABF}
\title{Plot the Result of readABF}
\description{
  Plot the result of \code{readABF}. For any ABF object \code{r} the result of \code{plot(r)} should be identical to that of \code{plot(as.data.frame(r))}.
}
\usage{
\method{plot}{ABF}(x, current=1, voltage=2, xlab="Time [s]", ylab="Conductance [nS]", \dots)
}
\arguments{
  \item{x}{the result of \code{readABF}.}
  \item{current}{a number indicating which column of the dataset contains the current.}
  \item{voltage}{a number indicating which column of the dataset contains the voltage.}
  \item{...}{further arguments to \code{plot.data.frame}}
}
\examples{
\dontrun{
# load some data and plot it
r <- readABF("some_data.abf")
plot(r)
}
}
\keyword{file}