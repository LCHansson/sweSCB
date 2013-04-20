#' @title rUtils.R
#' 
#' @description ...
#' 
#' @param queryData input vector

buildPath <- function(queryData = NULL) {
	paste(queryData, collapse="/")
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