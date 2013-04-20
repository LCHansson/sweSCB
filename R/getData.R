#' Get data
#' 
#' Get data from url
#' 
#' @param url url address
#' 
get_data <- function(url) {
    paste(readLines(url, warn = F), collapse="")
}
