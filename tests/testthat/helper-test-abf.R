examples <- function (name) {
   file.path("..", "..", "..", "Examples", name)
}

basic_test <- function (name, version, channel_names, channel_units, dims, mode) {
   r <- readABF(examples(name))
   # print(paste(name, r$header$nOperationMode))
   expect_equal(mode, r$header$nOperationMode)
   expect_equal(r$format_version, version)
   expect_equal(r$header$channel_names, channel_names)
   expect_equal(r$header$channel_units, channel_units)
   #expect_equal(class(r$data), "matrix") TODO: rethink, sometimes it is array
   expect_equal(dim(r$data), dims)
   #expect_equal(r$nSweeps, 1) TODO: THIS IS NOT TRUE AT ALL, it can be 0 or 1454 or...
}
