[![Travis-CI Build Status](https://travis-ci.org/Zabolekar/readABF.svg?branch=master)](https://travis-ci.org/Zabolekar/readABF)

## Installation

As of now, the package can be installed as follows:

```
library(devtools)
install_github("Zabolekar/readABF")
```

At some point it will be considered good enough and submitted to CRAN as well.

## Usage

```
library(readABF)
r <- readABF("some_data.abf")
plot(r)
```

## Difference from other packages

By "other packages" we mean [abf2](https://cran.r-project.org/web/packages/abf2/index.html) by Matthew Caldwell. While `abf2` only aims to read ABF2 files created by pClamp 10 and newer, we also aim to support ABF files created by pClamp 9 and older, especially ABF version 1.83.
