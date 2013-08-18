\name{scbGetData}
\alias{scbGetData}
\title{Get data from a bottom node in SCB API}
\usage{
  scbGetData(url, dims)
}
\arguments{
  \item{url}{URL to get data from (it is usually sufficient
  to submit the base URL, supplied via the baseURL()
  function, and the name of the variable).}

  \item{dims}{A list of dimensional parameters to filter
  data by. Note that values \emph{must} be submitted for
  all dimensions of the data. If you don't want to filter
  data, submit an asterisk in quotation marks ("*") instead
  of values for that dimension.}
}
\description{
  Get data from a bottom node in SCB API
}
\details{
  There are five documented filter types in the SCB API
  documentation; "Item", "All", "Top", "Agg" and "Vs". This
  function currently only supports the "Item" and "All"
  modes. To use "Item" selection, simply submit a value or
  vector of values with each dimensional parameter. To use
  "All" selection, submit a wildcard asterisk ("*") instead
  of a value.
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

## Inspect data
View(data)
}

