% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adorn.R
\name{cat_adorn_nobs}
\alias{cat_adorn_nobs}
\title{Adorning cats}
\usage{
cat_adorn_nobs(x, sep = ", ", n_string = "N = ")
}
\arguments{
\item{x}{a categorical variable (i.e., a factor)}

\item{sep}{a string that will be placed between the current levels
of `x` and the adorning sample size values.}

\item{n_string}{a string showing how you would like to indicate
that the number of observations in each group will be
presented. For example, if a group of size 10 is called `'A'`,
then using `n_string = 'N = '` will turn the category `'A'`
into `'A, N = 10'`}
}
\value{
a factor with new levels
}
\description{
For graphs or tables, categorical variables are often accompanied
  by sample sizes and corresponding proportions for each category.
  For example, group A had 10 participants, accounting for 50%
  of the A versus B study. These functions add descriptive
  counts and proportions to existing categorical variables
  so that presenting these data is easier.
}
\examples{

x <- factor(letters[c(1,1,1,1, 2,2,2,2)])
cat_adorn_nobs(x)

}
