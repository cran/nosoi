% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output_treeGenerator.r
\name{get_state}
\alias{get_state}
\title{Get State at sampling time}
\usage{
get_state(table.state, host, time, total.time)
}
\arguments{
\item{table.state}{data.table of hosts movement, extracted from a \code{nosoi} object}

\item{host}{ID of the host}

\item{time}{time of the sampling event}

\item{total.time}{total time of the epidemics, extracted from the \code{nosoi} object}
}
\value{
state of the host
}
\description{
Find the state of the host at the sampling time
}
\keyword{internal}
