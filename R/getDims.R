#' Get data dimensions form a bottom node in SCB API
#' 
#' Deparse and reorder data form the metadata object for pretty output
#' 
#' @param url URL to get data from
#' @param verbose Verbose output mode.
#' @export

scbGetDims <- function(url, verbose=TRUE) {
	mD <- getMetadata(url)
	
	## Deparse metadata object into elements
	# Title
	title <- mD$title$title
	
	vars <- mD$variables$variables
	ndim <- length(vars)
	
	names <- sapply(vars, function(var,i) { var$code }, 1:ndim)
# 	types <- sapply(vars, function(var,i) { var$text }, 1:ndim)
# 	
# 	times <- sapply(vars, function(var) "time" %in% names(var))
# 	eliminations <- sapply(vars, function(var) "elimination" %in% names(var))
	
	if(verbose) {
		cat("Title: \n", title, "\n")
		cat("Names: \n", names, "\n")
	}
	
	return(vars)
}