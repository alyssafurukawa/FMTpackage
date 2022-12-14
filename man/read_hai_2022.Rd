% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_hai_2022.R
\name{read_hai_2022}
\alias{read_hai_2022}
\title{Read HAI data}
\usage{
read_hai_2022(folder, quarter, year = 2022, joinMOU = FALSE)
}
\arguments{
\item{folder}{data location prior to quarter specification}

\item{quarter}{data quarter, used in path and elsewhere}

\item{year}{2022 by default}

\item{joinMOU}{whether the data should be joined the moulist file. needed for NA measures for non-respondents}
}
\description{
Read and combine HAI data files. An updated version of read_hai_2020 that reflects changes in data formatting.
}
