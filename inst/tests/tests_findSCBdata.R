# Testing the functions in the R package rSCB:
# file: findSCBdata.R
# require(testthat)
# test_file("inst/tests/tests_findSCBdata.R")
# test_package("rSCB")

cat("findSCBdata : ")

test_that(desc=".findScbData.inputBaseCat",{
  load("testFiles.Rdata")
  expect_output(.findScbData.inputBaseCat(1:2,test_codedAlt),
                "('q' = Quit, 'b' = Back)")
  expect_output(.findScbData.inputBaseCat(c(3,6),test_codedAlt),
                "('*' = Select all, 'a' = Show all)")

})

test_that(desc=".findScbData.printNode",{
  xscb <-data.frame(id=c("01","02","03"),
                    text=c("Värde 1","Värde 2", "Värde 3"))
  
  expect_output(.findScbData.printNode(xscb, print=TRUE),"Värde 3")
  expect_output(.findScbData.printNode(xscb, print=TRUE),"2. ")
  expect_that(.findScbData.printNode(xscb, print=FALSE),is_a("character"))
  expect_match(.findScbData.printNode(xscb, print=FALSE),"Värde 1")
})

test_that(desc=".findScbData.printCode",{
  varListText <- c("first","second","last") 

  expect_output(.findScbData.printCode(url="urladress", varListText, "namn", clean=TRUE),
                "urladress")
  expect_output(.findScbData.printCode(url="urladress", varListText, "namn", clean=TRUE),
                "namn")
  expect_output(.findScbData.printCode(url="urladress", varListText, "namn", clean=TRUE),
                "clean = TRUE")
  expect_output(.findScbData.printCode(url="urladress", varListText, "namn", clean=TRUE),
                "list\\(first")
})

test_that(desc=".findScbData.inputConvert",{
  expect_that(.findScbData.inputConvert(c("2","2:3","3:7","6")), 
              is_equivalent_to(c("2","3","4","5","6","7")))
  expect_that(.findScbData.inputConvert(c("4:5")), 
              is_equivalent_to(c("4","5")))
  expect_that(.findScbData.inputConvert(c("2","10:11","5")), 
              is_equivalent_to(c("2","5","10","11")))
  expect_that(.findScbData.inputConvert("*"), 
              is_equivalent_to("*"))  
})

test_that(desc=".findScbData.input",{
  cat("\n")
  expect_that(.findScbData.input(type="yesno","Enter 'y'"),is_equivalent_to("y"))
  cat("\n")
  expect_that(.findScbData.input(type="yesno","Enter 'n'"),is_equivalent_to("n"))
  cat("\n")
  expect_that(.findScbData.input(type="text","Enter 'MyData1'"),is_equivalent_to("MyData1"))
  cat("\n")

  testNode <- scbGetMetadata()
  cat("Enter: '3'")
  expect_that(.findScbData.input(type="node",testNode),
              is_equivalent_to("3"))
  cat("\n")
  cat("Enter: 'b'")
  expect_that(.findScbData.input(type="node",testNode),
              is_equivalent_to("b"))
  cat("\n")

  cat("Enter: '10:12, 1 ,3:1, 2'")
  test_varDF <- list(data.frame(id = as.character(seq(0.5,10,0.5)),
                      text = paste("Värde", as.character(seq(0.5,10,0.5))),
                      stringsAsFactors = FALSE),
                     "testingVärde")
  expect_that(.findScbData.input(type="alt", test_varDF),
              is_equivalent_to(c("1","2","3","10","11","12")))
  cat("\n")
})


cat("\n")

