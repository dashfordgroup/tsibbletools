% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequency.R
\name{change_frequency}
\alias{change_frequency}
\title{Change tsibble frequency}
\usage{
change_frequency(
  tsbl,
  freq = c("daily", "weekly", "monthly", "quarterly", "yearly"),
  rename_index = TRUE,
  .f = sum
)
}
\arguments{
\item{tsbl}{tsibble}

\item{freq}{frequency to apply to tsibble}

\item{.f}{summary function}
}
\value{
tsibble
}
\description{
Change tsibble frequency
}
