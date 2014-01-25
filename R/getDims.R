#' Get data dimensions form a bottom node in SCB API
#' 
#' Deparse and reorder data form the metadata object for pretty output
#' 
#' @param node Bottom node to deparse into object
#' @param verbose Verbose output mode.
#' @export

scbGetDims <- function(node, verbose=TRUE) {
	
	## Deparse metadata object into elements
	# Title
	title <- node$title$title
	
	vars <- node$variables$variables
	ndim <- length(vars)
	
	names <- sapply(vars, function(var,i) { var$code }, 1:ndim)
	
	if(verbose) {
		cat("Title: \n", title, "\n")
		cat("Names: \n", names, "\n")
	}
	
	return(vars)
}
