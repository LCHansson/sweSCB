#' Get data
#' 
#' Get data from url
#' 
#' @param url url address
#' 
getData <- function(url) {
    paste(readLines(url, warn = F), collapse="")
}
