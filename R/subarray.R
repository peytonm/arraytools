#' Subset an array based on names
#' 
#' \code{subarray} subsets an array based on dimension names, providing
#' a nice interface.
#' 
#' @param x the array to subset
#' @param ... subsetting instructions in the form of \code{dimname=values to keep}
#' @param drop whether to drop dimensions whenever possible
#' @return a subset of \code{x}
subarray <- function(x, ..., drop=FALSE) {
  indices <- rep(TRUE, length(dimnames(x)))
  call.indices <- list(...)
  for (dim.name in names(call.indices)) {
    indices[match(dim.name, names(dimnames(x)))] <- call.indices[dim.name]
  }
  args <- c(list(x), indices, drop=drop)
  do.call(`[`, args)
}