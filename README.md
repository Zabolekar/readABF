## Installation

As of now, the package can be installed as follows:

```
library(devtools)
install_github("Zabolekar/abfReader")
```

At some point it will be considered good enough and submitted to CRAN as well.

## Usage

```
library(abfReader)
r <- readABF("some_data.abf")
plot(r, current = 1, voltage = 2)
```

## Difference from other packages

By "other packages" we mean [abf2](https://cran.r-project.org/web/packages/abf2/index.html) by Matthew Caldwell. While `abf2` only aims to read ABF2 files created by pClamp 10 and newer, we also aim to support ABF files created by pClamp 9 and older, especially ABF version 1.83.