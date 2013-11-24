\name{scbCleanData}
\alias{scbCleanData}
\title{Tidies the data from SCB to R numeric/factor format in data.frame}
\usage{
  scbCleanData(data)
}
\arguments{
  \item{data}{data.frame object downloaded with
  \code{scbGetData}.}
}
\description{
  Tidies the data from SCB to R numeric/factor format in
  data.frame
}
\examples{
## Get data for a defined node
datanode <- scbGetMetadata("BefProgFoddaMedel10")

data <- scbGetData(datanode$url, dims=list(
   Fodelseland = "010",
 	Alder="*",
 	ContentsCode = "*",
 	Tid="*"
))

cleanData <- scbCleanData(data)

## Inspect data
View(cleanData)

## Plot data
barplot(cleanData$Födda.2010)
}

