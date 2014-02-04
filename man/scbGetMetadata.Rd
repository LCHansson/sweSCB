\name{scbGetMetadata}
\alias{scbGetMetadata}
\title{Get data}
\usage{
scbGetMetadata(path = NULL, quiet = TRUE, ...)
}
\arguments{
  \item{path}{URL to fetch metadata from. Defaults to the
  base URL of the SCB web API, which is equal to the top
  node in the data tree.}

  \item{quiet}{Quiet mode (never return a message to the
  user)}

  \item{...}{Further arguments passed to \code{baseURL()}.}
}
\description{
Get data from the API. If at the lowest node, provide the
user with a friendly message about this.
}
\examples{
## NOTE: DEPRECATED EXAMPLES!
# Define variable name
varname <- "BefProgFoddaMedel10"

# Get metadata
metadata <- scbGetMetadata(varname)

# Get dimensions (names of dimensions are printed in the terminal)
dims <- scbGetDims(metadata)
}

