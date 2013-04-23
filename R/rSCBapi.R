#' @title rSCBapi
#' 
#' @description A package for interfacing with the API of Statistics Sweden
#' (Statistiska Centralbyrån)
#' 
#' @section ...
#
#' @name rSCBapi
#' @docType package

scbQueryData <- list(
	base_url = "http://api.scb.se",
	api_name = "OV0104", # Are there any other names?
	api_version = "v1",
	database_id = "doris",
	language = "sv",
	levels = "ssd",
	table_id = "BE/BE0401/"
)

baseUrl <- buildPath(scbQueryData)



