## Installation

As of now, the package can be installed as follows:

```
library(devtools)
install_github("Zabolekar/ABFReader")
```

At some point it will be considered good enough and submitted to CRAN as well.

## Usage

```
library(ABFReader)
r <- readABF("some_data.abf")
plot(data.vs.time(r))
```

## Difference from other packages

By "other packages" we mean `abf2` by Matthew Caldwell. While `abf2` only aims to read ABF2 files created by pClamp 10 and newer, we also aim to support ABF files created by pClamp 9 and older, especially ABF version 1.83.