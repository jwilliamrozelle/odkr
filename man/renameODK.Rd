% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/renameODK.R
\name{renameODK}
\alias{renameODK}
\title{Rename column names of data exported from an ODK Aggregate Server or from
ODK Briefcase into more usable and human readable variable names.}
\usage{
renameODK(data, sep = c(".", "-"))
}
\arguments{
\item{data}{Dataframe object of dataset exported from ODK Aggregate Server
or from local ODK directory}

\item{sep}{Character value for separator used in variable names. Choices
are "." or "-". Default is ".".}
}
\value{
Data frame object with renamed variables
}
\description{
Rename column names of data exported from an ODK Aggregate Server or from
ODK Briefcase into more usable and human readable variable names.
}
\examples{
# Rename sampleData1 dataset to remove '.' from variable names
names(sampleData1)
renameODK(sampleData1)
names(sampleData1)

# Rename sampleData2 dataset
names(sampleData2)
renameODK(sampleData2)
names(sampleData2)

# Rename sampleData3 dataset
names(sampleData3)
renameODK(sampleData3)
names(sampleData3)

}
