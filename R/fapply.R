#' Apply a list of functions to a data frame
#'
#' @param x A data frame
#' @param flist A list of functions
#'
#' @return A modified data frame resulting from applying the functions in `f`.
#' @export
#'
#' @examples
#' fapply(iris[,1:4], list(function(x){x+1}, function(x){x-1}))
#' # This should result in iris[,1:4] as result (less rounding errors).
fapply <- function(x, flist, verbose = TRUE) {
  k <- length(flist)
  if ( k > 0 ) {
    for (i in 1:k) {
      if (verbose) cli::cli_bullets(c("*" = names(flist)[i]))
      x <- flist[[i]](x)
    }
  }
  x
}
