[![Travis-CI Build Status](https://travis-ci.org/Zabolekar/readABF.svg?branch=master)](https://travis-ci.org/Zabolekar/readABF) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/readABF)](https://cran.r-project.org/package=readABF)

## Installation

The package can be installed from CRAN in the usual manner:

```
install.packages("readABF")
```

Alternatively, you can install the latest version from GitHub:

```
library(devtools)
install_github("Zabolekar/readABF")
```

## Usage

```
library(readABF)
r <- readABF("some_data.abf")
plot(r)
```

## Difference to other packages

- [abf2](https://CRAN.R-project.org/package=abf2), an R package by Matthew Caldwell: while `abf2` only reads ABF2 files created by pClamp 10 and newer, we also support ABF files created by pClamp 9 and older, especially ABF version 1.83.

- [abfload](https://github.com/fcollman/abfload), a MATLAB function by Harald Hentschke, Forrest Collman and Ulrich Egert: we try to read at least everything that `abfload` is able to read. However, `abfload` had some updates in 2017 which we haven't incorporated so far.

## Tests

As you can notice, our `tests/` directory is not published, neither on GitHub nor on CRAN. This has two reasons:

- our test data is hundreds of megabytes large.
- we do not have the permission to share some of our test data.

This package is tested (currently only under Linux) with the following combinations of file format versions and operation modes (each with multiple data sets):

|    |variable-length event-driven|fixed-length event-driven|gap-free|episodic stimulation|
|----|:--------------------------:|:-----------------------:|:------:|:------------------:|
|1.65|                            |                         |        |yes                 |
|1.80|                            |                         |yes     |                    |
|1.83|                            |                         |yes     |yes                 |
|1.84|yes                         |yes                      |yes     |yes                 |
|2.00|no                          |yes                      |yes     |yes                 |

- "yes" means: it's tested and it works.
- "no" means: it isn't implemented yet and it gives an appropriate error message.
- empty cell means: it might work but we could not test it.

Episodic stimulation mode is also sometimes called waveform fixed-length mode. High-speed oscilloscope mode is not tested due to lack of sample files.

We would very much appreciate to receive further data sets for testing purposes. A repository to upload data sets can be provided at request.

## Acknowledgements

- Support of Deutsche Forschungsgemeinschaft grant SFB 803 Z2 is gratefully acknowledged.

- We would like to thank Harald Hentschke and his sources and the lab of Claudia Steinem for providing test data.
