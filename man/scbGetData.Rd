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
This function fetches actual data (i.e. values) from the
SCB web API.
}
\details{
There are five documented filter types in the SCB API
documentation; "Item", "All", "Top", "Agg" and "Vs". This
function currently only supports the "Item" and "All"
modes. To use "Item" selection, simply submit a value or
vector of values with each dimensional parameter. To use
"All" selection, submit a wildcard asterisk ("*") instead
of a value. For detailed examples of how the function can
be used, please refer to the manpage for
\code{\link{scbCleanData}}. Also see the installed example
files in the \code{examples} folder of
\code{path.package("rSCB")} (these are also viewable on the
project's GitHub page).
}
\seealso{
\code{\link{scbGetMetadata}}, \code{\link{scbGetDims}},
\code{\link{scbGetLevels}}, \code{\link{scbCleanData}}
}

