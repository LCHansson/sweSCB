#' Get data from a bottom node in SCB API
#' 
#' This function fetches actual data (i.e. values) from the SCB web API. 
#' 
#' @param url URL to get data from (it is usually sufficient to submit the base URL, supplied via the baseURL() function, and the name of the variable).
#' @param dims A list of dimensional parameters to filter data by. Note that values \emph{must} be submitted for all dimensions of the data. If you don't want to filter data, submit an asterisk in quotation marks ("*") instead of values for that dimension.
#' @param clean Clean and melt the data to R format.
#' 
#' @details
#' There are five documented filter types in the SCB API documentation; "Item", "All", "Top", "Agg" and "Vs". This function currently only supports the "Item" and "All" modes. 
#' To use "Item" selection, simply submit a value or vector of values with each dimensional parameter. To use "All" selection, submit a wildcard asterisk ("*") instead of a value.
#' For detailed examples see the installed example files in the \code{examples} folder of \code{path.package("rSCB")} (these are also viewable on the project's GitHub page).
#' 
#' @seealso
#' \code{\link{scbGetMetadata}}, \code{\link{scbGetDims}}, \code{\link{scbGetLevels}}
#' 
#' @export

scbGetData <- function(url, dims, clean = FALSE) {

   dimNames <- names(dims)
   
   queryBody <- list()
   
   # Define the query list
   for (i in 1:length(dims)) {
      if (length(dims[[dimNames[i]]]) == 1) {
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
   response <- try(POST(
      url = url,
      body = toJSON(list(
         query = queryBody,
         response = list(format = "csv")
      ))
   ), silent=TRUE)
   
   
   # Print error message
   if (class(response)=="try-error"){
      stop(str_join("No internet connection to ",url),
           call.=FALSE)
   }
   if(response$headers$statusmessage != "OK") {
     stop(str_join("Error in connection: ", response$headers$statusmessage),
          call.=FALSE)
   }
   
   # Parse data into human-readable form
   # (Due to a weird encoding issue on Windows this generates a warning
   # about faulty encoding. Hence the suppressMessages() encapsulation...)
   suppressMessages(a <- content(response, as="text"))
   b <- read.table(textConnection(a), sep=',', header=TRUE, stringsAsFactors=FALSE)
   
   # Clean and melt data 
   if (clean) {
      b <- .scbClean(b, url=url)
   }
   
   return(b)
}


.scbClean <- function(data2clean, url) {  
   
   # Temporary functions (only used in .scbClean)
   .applyFindLev <- function(vec, val) {
      # Function to create an integer vector with one integer per text in 'val' that
      # is found in vec
      resVec <- unlist(lapply(X=vec,
                              FUN=function(x,val) which(str_detect(as.character(x), val)),
                              val=val))
      return(resVec)
   }
   
   .cleanSCBcol <- function(x) {
      # Takes a character vector with numbers, remove all
      # spases and convert to numeric (if not x is a char vector)
      suppressWarnings(numx <- as.numeric(str_replace_all(x,"\\s","")))
      if(sum(is.na(numx)) == length(x)) {
         return(as.character(x))
      } else {
         return(numx)
      }
   }
   
   # Get metadata to use in creating factors of Tid and contentCode
   contentNode <- scbGetMetadata(path=str_split(url, "/")[[1]][length(str_split(url, "/")[[1]])])
   
   # Collect factor labels for tid and contentCode and convert
   # other variables to factor variables
   idvars <- character(0)
   for (content in contentNode$variables$variables) {
      if (content$code %in% c("Tid", "ContentsCode")) {
         assign(x = str_join("val", content$code, sep=""), content$values)
         assign(x = str_join("valText", content$code, sep=""), content$valueTexts)
         next()
      }
      varName <- content$text
      Encoding(varName) <- "UTF-8"
      varName <- make.names(varName)
      idvars <- c(idvars, varName)
      data2clean[, varName] <- as.factor(data2clean[, varName])
   }
   
   # Melt the data to long format
   meltData <- melt(data=data2clean, id.vars=make.names(idvars))
   
   # Add variables tid, tabellinnehåll and värde
   tidLev <- .applyFindLev(meltData$variable, valTextTid)
   tidLab <- valTextTid[sort(unique(tidLev))]
   meltData[, "tid"] <- factor(x=tidLev, labels=tidLab)
   
   contLev <- .applyFindLev(meltData$variable,make.names(valTextContentsCode))
   contLab <- valTextContentsCode[sort(unique(contLev))]
   meltData[, "tabellinneh\u00e5ll"] <- factor(x=contLev, labels=contLab)
   
   meltData[,"v\u00e4rde"] <- .cleanSCBcol(meltData$value)
   
   # Remove variables wiyhout any use
   meltData$value <- NULL
   meltData$variable <- NULL
   
   return(meltData)
}
