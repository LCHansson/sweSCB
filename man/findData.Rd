\name{findData}
\alias{findData}
\title{Find and download data from SCB}
\usage{
findData(history = FALSE, ...)
}
\arguments{
  \item{history}{keep the history when the function is
  running.}

  \item{...}{further parameters. These are currently
  ignored.}
}
\description{
Wrapper function (for \link{scbGetData} and
\link{scbGetMetadata}) to simply find and download data
from SCB to the current R session.
}
\examples{
\dontrun{
findData()
}
}
\seealso{
\code{\link{scbGetMetadata}}, \code{\link{scbGetData}}
}

