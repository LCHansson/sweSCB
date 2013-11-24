#' Get data
#' 
#' Get data from url
#' 
#' @param path URL to fetch metadata from. Defaults to the base URL of the SCB web API, which is equal to the top node in the data tree.
#' @param ... Further arguments passed to  \code{baseURL()}.
#' @export
#' 
scbGetMetadata <- function(path = NULL,...) {
	if(is.null(path))
		url <- baseURL(...)
	else
		url <- paste0(c(baseURL(...),path),collapse="/")
	
    df <- data.frame(
        t(sapply(
            RJSONIO::fromJSON(
                paste(readLines(url, warn = F), collapse = "")
            ),
            c
        ))
    )
	
	if("id" %in% names(df))
		df$id <- as.character(df$id)
	else {
		message("The data node returned is a bottom node.\nIf the name of your node object is `node`, call scbGetDims(node$URL) to get further information about the data node.")
		df$URL <- url
	}
	
	return(df)
}
