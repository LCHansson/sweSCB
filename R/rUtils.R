#' @title rUtils.R
#' 
#' @description ...
#' 
#' @param queryData input vector

buildPath <- function(queryData = NULL) {
	paste(queryData, collapse="/")
}