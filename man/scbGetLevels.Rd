\name{scbGetLevels}
\alias{scbGetLevels}
\title{Get levels from API node}
\usage{
scbGetLevels(descriptions = FALSE, quiet = FALSE, ...)
}
\arguments{
  \item{descriptions}{Whether to include node descriptions
  with the list of node IDs. (default: \code{FALSE})}

  \item{quiet}{Quiet mode. Whether to stop with an error if
  the input node does not contain any subnodes. If set to
  \code{TRUE}, the function will quietly return FALSE
  without any errors. (default: \code{FALSE})}

  \item{...}{further argument to send to
  \code{scbGetMetadata}}
}
\description{
Get levels from a node in the API. If at the lowest node,
return a warning.
}

