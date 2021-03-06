\name{scbGetDims}
\alias{scbGetDims}
\title{Get data dimensions form a bottom node in SCB API}
\usage{
scbGetDims(node, verbose = TRUE)
}
\arguments{
  \item{node}{Bottom node to deparse into object}

  \item{verbose}{Verbose output mode.}
}
\description{
Deparse and reorder data form the metadata object for
pretty output
}
\examples{
## CONTINUED FROM EXAMPLES IN scbGetMetadata()
# Get metadata for a named variable
url <- paste(c(baseURL(),"AM","AM0102","AM0102A","KLStabell14LpMan"), collapse="/")
metadata <- scbGetMetadata(url)

# Get dimensions (names of dimensions are printed in the terminal)
dims <- scbGetDims(metadata)

# Get data
test <- scbGetData(metadata$URL, dims=list(
   Myndighet = "C02",
   Kon = "*",
   Heltiddeltid = "*",
   ContentsCode = "*",
   Tid = "*"
))
}
\seealso{
\link{scbGetMetadata}
}

