#' Get data
#' 
#' Get data from the API. If at the lowest node, provide the user with a friendly message about this.
#' 
#' @param path URL to fetch metadata from. Defaults to the base URL of the SCB web API, which is equal to the top node in the data tree.
#' @param quiet Quiet mode (never return a message to the user)
#' @param ... Further arguments passed to  \code{baseURL()}.
#' @export
#' 
scbGetMetadata <- function(path = NULL, quiet=FALSE, ...) {
	if(is.null(path))
		url <- baseURL(...)
	else
		url <- paste0(c(baseURL(...),path),collapse="/")

    df <- try(
      data.frame(
        t(sapply(
            RJSONIO::fromJSON(
                paste(readLines(url, warn = F), collapse = "")
            ),
            c
        ))
    ),silent=TRUE
      )
  
	if(class(df)=="try-error"){
	  stop(str_join("No internet connection to ",url))
	}
	
	if("id" %in% names(df))
		df$id <- as.character(df$id)
	else {
		if(quiet){
			message("The data node returned is a bottom node.\nIf the name of your node object is `node`, call scbGetDims(node$URL) to get further information about the data node.")
		}
		df$URL <- url
	}
	
	return(df)
}
