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
# Get metadata for a variable
metadata <- scbGetMetadata("BefProgFoddaMedel10")

# Get dimensions (names of dimensions are printed in the terminal)
dims <- scbGetDims(metadata)

# Get data
test <- scbGetData(metadata$URL, dims=list(
   Fodelseland = "010",
   Alder="*",
   ContentsCode = "*",
   Tid="*"
))
}
\seealso{
\link{scbGetMetadata}
}

