# Testing the functions in the R package
# require(testthat)
# test_file("inst/tests/tests.R")
# test_package("rSCB")

require(testthat)

test_that(desc=".findScbData.inputBaseCat works",{
  load("testFiles.Rdata")
  expect_output(.findScbData.inputBaseCat(1:2,test_codedAlt),
                "('q' = Quit, 'b' = Back)")
  expect_output(.findScbData.inputBaseCat(c(3,6),test_codedAlt),
                "('*' = Select all, 'a' = Show all)")

})

test_that(desc=".findScbData.printNode works",{
  xscb <-data.frame(id=c("01","02","03"),
                    text=c("Värde 1","Värde 2", "Värde 3"))
  
  expect_output(.findScbData.printNode(xscb, print=TRUE),"Värde 3")
  expect_output(.findScbData.printNode(xscb, print=TRUE),"2. ")
  expect_that(.findScbData.printNode(xscb, print=FALSE),is_a("character"))
  expect_match(.findScbData.printNode(xscb, print=FALSE),"Värde 1")
})

test_that(desc=".findScbData.printNode works",{
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



