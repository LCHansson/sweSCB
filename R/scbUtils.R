#' Build a path from character elements
#' 
#' This function takes a list if strings and builds a URL to the SCB web API \emph{in reverser order}.
#' 
#' @param varname The name of the variable in the web API. This can be a data node or a tree node.
#' @param topnodes A string or a list of strings containing the top nodes \emph{in top-to-bottom order}
#' @param baseURL The base URL to use. This is only useful if you want to use the function for constructing a URL to another web service or SCB suddenly should change their base URL.

buildPath <- function(varname, topnodes = NULL, base = baseURL()) {
	# Error handling
	if(topnodes == "")
		stop("ERROR: Internal function rSCB:::buildPath: `topnodes` argument set to empty string\n
			 The `topnodes` argument is required to be either NULL or a value or a vector other than [''] interpretable as a character string by paste().\n")
	
	# Clean URL string: remove trailing slash
	base <- str_replace_all(base,"/$","")
	
	# Clean topnodes string: Remove whitespace and leading/trailing slashes
	topnodes <- str_trim(topnodes)
	topnodes <- str_replace_all(topnodes,"^/|/$","")
	
	# Build a vector, in the right order, for the URL elements
	urlElements <- c(base,topnodes,varname)
	
	# Create full path and return it
	return(
		paste0(urlElements, collapse="/")
	)
}

#' Get content from response
#' 
#' Get the content from a response object
#' 
#' @param response response object
#' @param type type format
#' 
getContent <- function(response, type = "csv") {
    
    if (!class(response) == "response") {
        stop("needs to be an response class object")
    }
    
    # Convert to character
    content <- httr::content(response)
    
    if (type == "csv") {
        content <- read.table(
            textConnection(content), 
            sep = ",", 
            header = T, 
            stringsAsFactors = F
        )
    } else {
        stop("Unsupported type format")
    }
    
    return(content)
}

#' Return base URL to SCB API
#' 
#' ...
#' 
#' @param version The version of SCB API to use. (Default: \code{v1})
#' @export
baseURL <- function(version="v1") {
	paste(sprintf("http://api.scb.se/OV0104/%s/doris/sv/ssd",version))
}