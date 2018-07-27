\name{conductance}
\alias{conductance}
\title{Compute a Data Frame with Time and Conductance}
\description{
  Compute a data frame with time and conductance.
}
\usage{
conductance(x, sweep, current, voltage)
}
\arguments{
  \item{x}{the result of \code{readABF}.}
  \item{sweep}{a number indicating which sweep to convert, if there are multiple sweeps.}
  \item{current}{a number indicating which channel contains the current.}
  \item{voltage}{a number indicating which channel contains the voltage.}
}
\examples{
\dontrun{
# load some data and plot it
r <- readABF("some_data.abf")
conductance(r)
}
}
\keyword{file}