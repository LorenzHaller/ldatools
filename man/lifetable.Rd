% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/life-table.R
\name{lifetable}
\alias{lifetable}
\title{Implementation of the actuarial life table method.}
\usage{
lifetable(data, breaks = NULL, time_var = "time", status_var = "status",
  right_closed = FALSE, max_end = FALSE)
}
\arguments{
\item{data}{Data set from which time and status variables will be extracted.}

\item{breaks}{The time points of the interval borders.}

\item{time_var}{The variable storing event times.}

\item{status_var}{The variable storing the event indicator.}

\item{right_closed}{Logical. If \code{TRUE} (default), intervals are assumed right_closed
closed and left open. If \code{FALSE} left closed and right_closed open.}

\item{max_end}{logical. Should the last interval span until the last
observed censoring or event time (if larger than the largest specified
cut point).}
}
\value{
A \code{\link[tibble]{tibble}} containing riskset information as well
as hazard and survival function calculated by using the actuarial life table
method.
}
\description{
Implementation of the actuarial life table method.
}
\examples{
data("Melanoma", package="MASS")
Melanoma$status <- ifelse(Melanoma$status == 1, 1, 0)
lifetable(Melanoma, breaks = seq(0, 6000, 1500))
}
\seealso{
split_info
}
