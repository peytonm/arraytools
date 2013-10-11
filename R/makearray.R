#' Easily create an array
#' 
#' \code{makearray} creates an array, intelligently collecting and assigning
#' dimension lengths and names from its \code{...} arguments. An argument can
#' either be a single number, in which case it is taken to be a dimension length,
#' or it can be a vector of values, in which case the values specify names along
#' that dimension. If a dimension argument is -1, the length of that dimension
#' will be inferred based on the length of the data and the length of the other
#' dimensions. The order of dimension arguments will match the order of
#' the dimensions in the created array.
#' 
#' Note that if you wish to create a dimension of length one with a ``numeric'' name,
#' you must pass the name in as a character vector. A numeric vector of length one is
#' always taken to be a dimension length.
#' 
#' @param data the array's data
#' @param ... dimension information, as described above
#' @return an array with the specified dimension lengths and names
makearray <- function(data=NA, ...) {
  dim.info <- list(...)
  dims <- numeric()
  dimnames <- vector("list", length(dim.info))
  names(dimnames) <- names(dim.info)
  
  for (i in seq_along(dim.info)) {
    if (is.numeric(dim.info[[i]]) && length(dim.info[[i]]) == 1) {
      # We've been passed a dimension length
      dims[i] <- dim.info[[i]]
      dimnames[[i]] <- NULL
    } else {
      # We have dimnames
      dims[i] <- length(dim.info[[i]])
      dimnames[[i]] <- dim.info[[i]]
    }
  }
  
  if (any(dims == -1)) {
    if (sum(dims == -1) > 1) {
      stop("Ambiguous dimensions.")
    }
    dims[dims == -1] <- length(data)/prod(dims[dims != -1])
  }
  
  if (prod(dims) != length(data)) {
    stop("Dimensions inconsistent with data.")
  }
  
  array(data, dims, dimnames)
}