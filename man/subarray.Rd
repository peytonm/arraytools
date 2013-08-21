\name{subarray}
\alias{subarray}
\title{Subset an array based on names}
\usage{
  subarray(x, ..., drop = FALSE)
}
\arguments{
  \item{x}{the array to subset}

  \item{...}{subsetting instructions in the form of
  \code{dimname=values to keep}}

  \item{drop}{whether to drop dimensions whenever possible}
}
\value{
  a subset of \code{x}
}
\description{
  \code{subarray} subsets an array based on dimension
  names, providing a nice interface.
}

