\name{readABF}
\alias{readABF}
\title{Reads ABF Files}
\description{
Reads ABF2 files, created by pClamp 10 and newer, and ABF files, created by pClamp 9 and older.
}
\usage{
readABF(file)
}

\arguments{
  \item{file}{character vector, containing the file name or path.}
}

\value{an ABF object, i.e. a \code{\link{list}} containing the data and other information like the header.}

\seealso{
  \code{\link{as.data.frame.ABF}}, \code{\link{plot.ABF}}, \code{\link{print.ABF}}
}

\examples{
## reads an example file
readABF(system.file("extdata", "2009_01_19_0002_varlen_v18.abf", package="readABF"))
}
