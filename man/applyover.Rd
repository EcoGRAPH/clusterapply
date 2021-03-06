% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/applyover.R
\name{applyover}
\alias{applyover}
\title{Apply a function over a dataframe after splitting along some "over" variable}
\usage{
applyover(
  applyfun = NULL,
  applyargs = NULL,
  fallbackargs = NULL,
  settosplit = NULL,
  nameaftersplit = NULL,
  over = NULL
)
}
\arguments{
\item{applyfun}{The character name of a function to apply.}

\item{applyargs}{A named list of arguments supplied to applyfun.}

\item{fallbackargs}{A named list of arguments to replace arguments in
applyargs if the call to applyfun results in an error. Any arguments
in applyargs that are not named in fallbackargs will be reused.}

\item{settosplit}{A dataframe that will be split by the 'over'
parameter below.}

\item{nameaftersplit}{The name of the argument of applyfun that
takes the settosplit dataframe after it has been split; this is
commonly 'x' or 'data'}

\item{over}{The name of the factor variable in settosplit, over
which it should be split.}
}
\value{
This function returns a named list of results, having applied
  applyfun to settosplit for every level of the 'over' variable. So for
  example, applyover(...)[["a"]] is the result of applyfun(data[data$over == 'a']).
}
\description{
Apply a function over a dataframe after splitting along some "over" variable
}
