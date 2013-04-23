#' Get levels from API node
#'
#' Get levels from a node in the API. If at the lowest node, return error.
#'
#' @param basePath input URL to node

#' Get levels from node in
getLevels <- function(baseUrl = NULL) {
	
	if(is.null(baseUrl)) {
		stop("no URL to parse")
	}
	
	nodeData <- getMetadata(baseUrl)
	
	if(!("id" %in% names(nodeData))) {
		stop("already at lowest node, fetch data instead")
	}
	
	ids <- nodeData$id
	
	return(ids)
}