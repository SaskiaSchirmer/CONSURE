% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rename_data.R
\name{rename_data}
\alias{rename_data}
\title{rename recovery data with default column names}
\usage{
rename_data(recovery_data, xname, yname = NULL, timename, mark_area_name)
}
\arguments{
\item{recovery_data}{data.frame with dead recoveries, containing information
on the area of origin, geographical coordinates of recovery and age at death}

\item{xname}{character, name of column containing information on longitude,
specify this in one-dimensional case}

\item{yname}{character, name of column containing information on latitude,
defaults to NULL, do not specify in one-dimensional case, but use xname
instead}

\item{timename}{character, name of column containing information on age at
death}

\item{mark_area_name}{character, name of column containing information on
the area of origin}
}
\value{
recovery_data with default column names
}
\description{
This function renames the recovery data with default column names.
}
\examples{
{
    rename_data(recovery_data = robins_init$recovery_data,
               xname        = "recLon",
               yname        = "recLat",
               timename     = "age",
               mark_area_name = "mark_area")
}
}
