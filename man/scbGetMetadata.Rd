\name{scbGetMetadata}
\alias{scbGetMetadata}
\title{Get data}
\usage{
scbGetMetadata(path = NULL, node = NULL, topnodes = NULL, quiet = TRUE,
  ...)
}
\arguments{
  \item{path}{URL to fetch metadata from. If left empty,
  the function constructs a URL from the \code{node} and
  \code{topnodes} arguments}

  \item{node}{A string with the name of the node to fetch.
  This is ignored if \code{path} is supplied.}

  \item{topnodes}{A string or list of strings with the
  names of the parent nodes of \code{node}. This is ignored
  if \code{path} is supplied.}

  \item{quiet}{Quiet mode (never return a message to the
  user)}

  \item{...}{Further arguments passed to \code{baseURL()}.}
}
\description{
Get data from the API. If at the lowest node, provide the
user with a friendly message about this.
}
\examples{
# Define variable name
topnode <- scbGetMetadata()

# Get metadata for the first element in the top node
nextnode <- scbGetMetadata(topnode$URL[1])

# Get metadata for a named node with named topnodes
a_node <- scbGetMetadata()
}

