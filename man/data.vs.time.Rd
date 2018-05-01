\name{data.vs.time}
\alias{data.vs.time}
\title{Prepare the result of readABF for plotting}
\description{
  Convert the result of \code{readABF} to a data frame that can be easily plotted.
}
\usage{
data.vs.time(r, current=1, voltage=2)
}
\arguments{
  \item{r}{the result of \code{readABF}.}
  \item{current}{a number indicating which column of the dataset contains the current.}
  \item{voltage}{a number indicating which column of the dataset contains the voltage.}
}
\examples{
\dontrun{
# load some data and plot it
r <- readABF("some_data.abf")
plot(data.vs.time(r))
}
}
\keyword{file}