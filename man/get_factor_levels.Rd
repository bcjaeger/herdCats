% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.R
\name{get_factor_levels}
\alias{get_factor_levels}
\title{factor levels}
\usage{
get_factor_levels(data, ...)
}
\arguments{
\item{data}{a data frame containing factors.}

\item{...}{factor variables in `data` to obtain factor levels for.
If unspecified, all factor variables are considered. This argument
is compatible with `tidyselect` commands.}
}
\description{
Using [cat_gather] requires factor levels to be supplied in the form
  of a list. The names of the list should correspond to names of
  factors. The values of the list should be the levels of the factor.
  This function creates that list for you.
}
