#' Get levels from API node
#'
#' Get levels from a node in the API. If at the lowest node, return error.
#'
#' @param basePath input URL to node

#' Get levels from node in
getLevels <- function(baseUrl = NULL,categoryDescriptions = FALSE,returnError=T) {
	
	if(is.null(baseUrl)) {
		if(returnError) stop("no URL to parse")
		if(!returnError) return(FALSE)
	}
	
	nodeData <- getMetadata(baseUrl)
	
	if(!("id" %in% names(nodeData))) {
		if(returnError) stop("already at lowest node, fetch data instead")
		if(!returnError) return(FALSE)
	}
	
	if(!categoryDescriptions) {
		ids <- list(id=nodeData$id)
	} else {
		ids <- list(id=nodeData$id,
					description=nodeData$text
		)
	}
	
	return(ids)
}

#' Function to silently test for existence of metadata and return TRUE or FALSE
#' based on the result of that test.
checkForLevels <- function(url = NULL) {
	
	if(is.null(url)) {
		return(FALSE)
	}
	
	nodeData <- getMetadata(url)
	if(!("id" %in% names(nodeData))) {
		return(FALSE)
	}
	
	# If the data passed both tests, return TRUE
	return(TRUE)
}