#' get number of NAs in a vector (complement to n_valid())
#'
#' @param x a vector
#'
#' @return a numeric vector
#' @export
#'
#' @examples
n_missing <- function(x){

  assertthat::assert_that(base::is.vector(x))

  sum(is.na(x))

}
