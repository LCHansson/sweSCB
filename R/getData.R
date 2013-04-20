#' Get data
#' 
#' Get data from url
#' 
#' @param url url address
#' 
getData <- function(url) {
    data.frame(
        t(sapply(
            RJSONIO::fromJSON(
                paste(readLines(url, warn = F), collapse = "")
            ),
            c
        ))
    )
}
