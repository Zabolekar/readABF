context("Malformed files")

test_that("We can't read malformed files", {
   expect_error(readABF(examples("atfinfo.txt")), "unknown or incompatible file signature")
   expect_error(readABF(examples("myconverter.m")), "unknown or incompatible file signature")
})
