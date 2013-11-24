#' Tidies the data from SCB to R numeric/factor format in data.frame
#' 
#' @param data data.frame object downloaded with \code{scbGetData}.
#' 
#' @examples
#' ## Get data for a defined node
#' datanode <- scbGetMetadata("BefProgFoddaMedel10")
#' 
#' data <- scbGetData(datanode$url, dims=list(
#'    Fodelseland = "010",
#'  	Alder="*",
#'  	ContentsCode = "*",
#'  	Tid="*"
#' ))
#' 
#' cleanData <- scbCleanData(data)
#' 
#' ## Inspect data
#' View(cleanData)
#' 
#' ## Plot data
#' barplot(cleanData$Födda.2010)
#' 
#' @export

scbCleanData <- function(data) {
	as.data.frame(
		lapply(
			X=data,
			FUN=.cleanSCBcol
		),
		stringsAsFactors=TRUE
	)
}

#' Internal function to clean data
#' 
#' "Cleans" data by checking if it's entirely numeric (integer or float) and then returning it in numeric form. Otherwise tevector is returned as character.
#' 
#' @param x The vector to clean (i.e. )

.cleanSCBcol<-function(x) {
	suppressWarnings(numx <- as.numeric(str_replace_all(x,"\\s","")))
	
	if(sum(is.na(numx)) == length(x)) {
		return(as.character(x))
	} else {
		return(numx)
	}
}
