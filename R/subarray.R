.get.subset.args <- function(x, ...) {
  indices <- rep(TRUE, length(dimnames(x)))
  call.indices <- list(...)
  for (dim.name in names(call.indices)) {
    indices[match(dim.name, names(dimnames(x)))] <- call.indices[dim.name]
  }
  args <- c(list(x), indices)
}

#' Subset an array based on names
#' 
#' \code{subarray} subsets an array based on dimension names, providing
#' a nice interface. It can be used to obtain the subset, or to assign
#' to the subset.
#' 
#' @param x the array to subset
#' @param ... subsetting instructions in the form of \code{dimname=values to keep}
#' @param drop whether to drop dimensions whenever possible
#' @return When not used as part of an assignment, \code{subarray} will return the
#' proper subset of \code{x}.
#' @rdname subarray
subarray <- function(x, ..., drop=FALSE) {
  args <- .get.subset.args(x, ...)
  args$drop <- drop
  do.call(`[`, args)
}

#' @param value the value to assign to the subset
#' @rdname subarray
`subarray<-` <- function(x, ..., value) {
  args <- .get.subset.args(x, ...)
  args$value <- value
  do.call(`[<-`, args)
}