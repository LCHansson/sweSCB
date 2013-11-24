\name{cleanSCB}
\alias{cleanSCB}
\title{Tidies the data from SCB to R numeric/factor format in data.frame}
\usage{
  cleanSCB(data)
}
\arguments{
  \item{data}{data.frame object downloaded with the
  rSCBapi.}
}
\description{
  Tidies the data from SCB to R numeric/factor format in
  data.frame
}
\examples{
## Get data for a defined node
url <- buildPath(baseURL(),"BE/BE0401/BE0401B/BefProgFoddaMedel10")
# alternatively:
url <- buildPath(baseURL(),"BefProgFoddaMedel10")

data <- scbGetData(url, dims=list(
   Fodelseland = "010",
 	Alder="*",
 	ContentsCode = "*",
 	Tid="*"
))

cleanData <- cleanSCB(data)

## Inspect data
View(cleanData)
}

