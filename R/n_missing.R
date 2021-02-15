#' get number of NAs in a vector (complement to n_valid())
#'
#' @param x a vector
#'
#' @return
#' @export
#'
#' @examples
n_missing <- function(x){

  sum(is.na(x))

}
