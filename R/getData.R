#' Get data from a bottom node in SCB API
#' 
#' This function fetches actual data (i.e. values) from the SCB web API. 
#' 
#' @param url URL to get data from (it is usually sufficient to submit the base URL, supplied via the baseURL() function, and the name of the variable).
#' @param dims A list of dimensional parameters to filter data by. Note that values \emph{must} be submitted for all dimensions of the data. If you don't want to filter data, submit an asterisk in quotation marks ("*") instead of values for that dimension.
#' 
#' @details
#' There are five documented filter types in the SCB API documentation; "Item", "All", "Top", "Agg" and "Vs". This function currently only supports the "Item" and "All" modes. 
#' To use "Item" selection, simply submit a value or vector of values with each dimensional parameter. To use "All" selection, submit a wildcard asterisk ("*") instead of a value.
#' For detailed examples of how the function can be used, please refer to the manpage for \code{\link{scbCleanData}}. Also see the installed example files in the \code{examples} folder of \code{path.package("rSCB")} (these are also viewable on the project's GitHub page).
#' 
#' @seealso
#' \code{\link{scbGetMetadata}}, \code{\link{scbGetDims}}, \code{\link{scbGetLevels}}, \code{\link{scbCleanData}}
#' 
#' @export

scbGetData <- function(url, dims) {
	dimNames <- names(dims)
	
	queryBody <- list()
	
	# Define the query list
	for(i in 1:length(dims)) {
		if(length(dims[[dimNames[i]]]) == 1) {
			filter = ifelse(dims[[dimNames[i]]] == "*", "all", "item")
		} else {
			filter = "item"
		}
		
		queryBody[[i]] <- list(
			code = dimNames[i],
			selection = list(filter = filter,
							 values = as.list(dims[[dimNames[i]]])
		))
	}
	
	# Get data
	response <- POST(
		url=url,
		body=toJSON(list(
			query = queryBody,
			response = list(format = "csv")
		))
	)
	
	# Parse data into human-readable form
	a <- content(response, as="text")
	b <- read.table(textConnection(a), sep=',', header=T, stringsAsFactors=F)
	
	return(b)

}
