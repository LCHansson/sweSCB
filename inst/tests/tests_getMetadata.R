# Testing the functions in the R package rSCB:
# file: getMetaData.R
# require(testthat)
# test_file("inst/tests/tests.R")
# test_package("rSCB")

cat("getMetaData : ")

test_that(desc="getMetadata works",{
  testFile<-scbGetMetadata()
  expect_that(testFile,is_a("data.frame"))
  expect_that(dim(testFile),is_equivalent_to(c(21,3)))
})

cat("\n")
