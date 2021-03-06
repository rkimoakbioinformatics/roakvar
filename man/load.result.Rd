% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roakvar.R
\name{load.result}
\alias{load.result}
\title{load.result}
\usage{
load.result(dbpath, level = "variant", quiet = TRUE)
}
\arguments{
\item{dbpath}{OakVar result database file}

\item{level}{Level to fetch result for. 'variant', 'gene', 'sample', or 'mapping', for example}

\item{quiet}{Run quietly}
}
\value{
A list. Fetched result
}
\description{
load.result
}
\examples{
# Fetch the variant level result from the result database exampleinput.sqlite.
#roakvar::load.result("exampleinput.sqlite", level='variant')

}
