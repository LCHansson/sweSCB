#' Tidies the data from SCB to R numeric/factor format in data.frame
#' 
#' @param data data.frame object downloaded with the rSCBapi.
#' 
#' @examples
#' ## Get data for a defined node
#' url <- buildPath(baseURL(),"BE/BE0401/BE0401B/BefProgFoddaMedel10")
#' # alternatively:
#' url <- buildPath(baseURL(),"BefProgFoddaMedel10")
#' 
#' data <- scbGetData(url, dims=list(
#'    Fodelseland = "010",
#'  	Alder="*",
#'  	ContentsCode = "*",
#'  	Tid="*"
#' ))
#' 
#' cleanData <- cleanSCB(data)
#' 
#' ## Inspect data
#' View(cleanData)
#' 
#' @export

cleanSCB <- function(data) {
	as.data.frame(
		apply(
			X=data,
			MARGIN=2,
			FUN=.cleanSCBcol
		),
		stringsAsFactors=TRUE
	)
}

.cleanSCBcol<-function(x) {
	suppressWarnings(numx <- as.numeric(str_replace_all(x,"\\s","")))
	
	if(sum(is.na(numx)) == length(x)) {
		return(as.character(x))
	} else {
		return(numx)
	}
}
