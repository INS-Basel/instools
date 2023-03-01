#' get sum of valid (not NAs) of a vector
#'
#' @param x a vector
#'
#' @return an integer value
#' @export
#'
n_valid <- function(x){

  # check inputs
  assertthat::assert_that(base::is.vector(x))

  sum(!is.na(x))

}
