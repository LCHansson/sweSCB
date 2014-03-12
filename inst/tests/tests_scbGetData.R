# Testing the functions in the R package rSCB:
# file: SCBdata.R
# require(testthat)
# test_file("inst/tests/tests_scbGetData.R")
# load("inst/tests/testFiles.Rdata")
# test_package("sweSCB")

cat("scbGetData : ")

test_that(desc="scbGetData",{
  testData <- 
    scbGetData(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
               dims = list(Region = c('00', '01'),
                           Civilstand = c('*'),
                           Alder = c('0', 'tot'),
                           Kon = c('*'),
                           ContentsCode = c('BE0101N1'),
                           Tid = c('2010', '2011', '2012', '2013')),
               clean = FALSE)  
  expect_equal(object=dim(testData), c(32,8))
  expect_equal(object=class(testData), "data.frame")
  testData2 <- 
    scbGetData(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet",
               dims = list(ContentsCode = c('PR0101A1'),
                           Tid = c('*')),
               clean = TRUE)
  expect_equal(object=class(testData2), "data.frame")
  expect_equal(object=ncol(testData2), 3)
})

cat("\n.scbClean : ")

test_that(desc="scbGetData",{
  cleanTestData <-
    scbGetData(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
               dims = list(Region = c('00', '01'),
                           Civilstand = c('*'),
                           Alder = c('0', 'tot'),
                           Kon = c('*'),
                           ContentsCode = c('BE0101N1'),
                           Tid = c('2010', '2011', '2012', '2013')),
               clean = TRUE)  
  
  expect_equal(object=dim(cleanTestData), c(128,7))
  expect_equal(object=class(cleanTestData), "data.frame")
  expect_equal(object=sum(is.na(cleanTestData[,1])), 0)
  expect_is(object=cleanTestData[,1], "factor")
  expect_equal(object=sum(is.na(cleanTestData[,2])), 0)
  expect_is(object=cleanTestData[,2], "factor")
  expect_equal(object=sum(is.na(cleanTestData[,3])), 0)
  expect_is(object=cleanTestData[,3], "factor")
  expect_equal(object=sum(is.na(cleanTestData[,4])), 0)
  expect_is(object=cleanTestData[,4], "factor")
  
  cleanTestData <-
    scbGetData(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0114/LCIArbKv",
               dims = list(SNI2007 = c('*'),
                           ContentsCode = c('*'),
                           Tid = c('*')),
               clean = FALSE)
  
  
})

