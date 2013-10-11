\name{makearray}
\alias{makearray}
\title{Easily create an array}
\usage{
  makearray(data = NA, ...)
}
\arguments{
  \item{data}{the array's data}

  \item{...}{dimension information, as described above}
}
\value{
  an array with the specified dimension lengths and names
}
\description{
  \code{makearray} creates an array, intelligently
  collecting and assigning dimension lengths and names from
  its \code{...} arguments. An argument can either be a
  single number, in which case it is taken to be a
  dimension length, or it can be a vector of values, in
  which case the values specify names along that dimension.
  If a dimension argument is -1, the length of that
  dimension will be inferred based on the length of the
  data and the length of the other dimensions. The order of
  dimension arguments will match the order of the
  dimensions in the created array.
}
\details{
  Note that if you wish to create a dimension of length one
  with a ``numeric'' name, you must pass the name in as a
  character vector. A numeric vector of length one is
  always taken to be a dimension length.
}

