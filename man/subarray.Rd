\name{subarray}
\alias{subarray}
\alias{subarray<-}
\title{Subset an array based on names}
\usage{
  subarray(x, ..., drop = FALSE)

  subarray(x, ..., value) <- value
}
\arguments{
  \item{x}{the array to subset}

  \item{...}{subsetting instructions in the form of
  \code{dimname=values to keep}}

  \item{drop}{whether to drop dimensions whenever possible}

  \item{value}{the value to assign to the subset}
}
\value{
  When not used as part of an assignment, \code{subarray}
  will return the proper subset of \code{x}.
}
\description{
  \code{subarray} subsets an array based on dimension
  names, providing a nice interface. It can be used to
  obtain the subset, or to assign to the subset.
}

