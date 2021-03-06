\name{buildPath}
\alias{buildPath}
\title{Build a path from character elements}
\usage{
buildPath(varname, topnodes = NULL, baseUrl = NULL, ...)
}
\arguments{
  \item{varname}{A character string or a list of strings of
  the variable(s) in the web API. This can be a data node
  or a tree node.}

  \item{topnodes}{A string or a list of strings containing
  the top nodes \emph{in top-to-bottom order}}

  \item{baseUrl}{The base URL to use. This is only useful
  if you want to use the function for constructing a URL to
  another web service or if SCB should suddenly change
  their base URL. If you want to pass arguments to
  \code{baseURL()}, use the \code{...} argument instead.}

  \item{...}{Further arguments passed to \code{baseURL()}.}
}
\description{
This function takes a list of strings and builds a URL to
the SCB web API \emph{in reverser order}.
}

