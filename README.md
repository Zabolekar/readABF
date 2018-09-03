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

## Tests

As you can notice, the `tests/` directory is included in both `.Rbuildignore` and `.gitignore`, so it isn't available from CRAN nor from GitHub. You may wonder why. This has two reasons:

- our test data is hundreds of megabytes large, it wouldn't be wise to upload it to CRAN anyway.
- it's not our data, and we didn't obtain permission to share all of it.

So, for now, our tests aren't publicly available, sorry.

This package is being tested with the following combinations of file format versions and operation modes (and, of course, with different and differently-shaped data):

|    |variable-length event-driven|fixed-length event-driven|gap-free|episodic stimulation|
|----|:--------------------------:|:-----------------------:|:------:|:------------------:|
|1.65|                            |                         |        |yes                 |
|1.80|                            |                         |yes     |                    |
|1.83|                            |                         |yes     |yes                 |
|1.84|yes                         |yes                      |yes     |yes                 |
|2.00|no                          |yes                      |yes     |yes                 |

- "yes" means: it's tested and it works.
- "no" means: it isn't implemented yet but we've tested that it gives the correct error message.
- empty cell means: it should work but it isn't tested.

Episodic stimulation mode is also sometimes called waveform fixed-length mode. High-speed oscilloscope mode wasn't tested due to lack of sample files.
