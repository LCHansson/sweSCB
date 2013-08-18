\name{scbGetLevels}
\alias{scbGetLevels}
\title{Get levels from API node}
\usage{
  scbGetLevels(baseUrl = NULL,
    categoryDescriptions = FALSE, returnError = TRUE)
}
\arguments{
  \item{baseUrl}{input URL to node (default: \code{NULL})}

  \item{categoryDescriptions}{whether to include node
  descriptions with the node IDs (default: \code{FALSE})}

  \item{returnError}{whether to stop with an error if the
  input node does not contain any subnodes. If set to
  \code{FALSE}, the function will simply exit silently.
  (default: \code{TRUE})}
}
\description{
  Get levels from a node in the API. If at the lowest node,
  return error.
}

