#' Build a path from character elements
#' 
#' ...
#' 
#' @param queryData input vector
#' 

buildPath <- function(...) {
	urlElements <- c(...)
	paste0(urlElements, collapse="/")
}

#' Get content from response
#' 
#' Get the content from a response object
#' 
#' @param response response object
#' @param type type format
#' 
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