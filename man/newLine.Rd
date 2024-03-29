% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nosoi_tablesManagment.R
\name{newLine}
\alias{newLine}
\title{Creates a new line to be added to the table when new host is infected (internal function)}
\usage{
newLine(
  hosts.ID,
  infected.by,
  infected.in,
  time.is,
  ParamHost,
  current.environmental.value = NULL,
  current.cell.number.raster = NULL,
  current.count.A = integer(0),
  current.count.B = integer(0)
)
}
\arguments{
\item{hosts.ID}{unique ID for the new host}

\item{infected.by}{unique ID of host that transmits to the new one}

\item{infected.in}{state in which the host was infected}

\item{time.is}{time in the simulation, when the infection takes place}

\item{ParamHost}{list of individual based parameters.}

\item{current.environmental.value}{current environmental value}

\item{current.cell.number.raster}{unique number of the raster cell where the host is}

\item{current.count.A}{current count of host A}

\item{current.count.B}{current count of host B}
}
\value{
a list with the new line to add.
}
\description{
This function creates a new line for the table.
The lines are to be bounded with \code{\link[data.table]{rbindlist}}.
}
\keyword{internal}
