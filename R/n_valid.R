#' get sum of valid (not NAs) of a vector
#'
#' @param x a vector
#'
#' @return \code{n.valid()} returns the number of fields not being NA in a vector
#' @export
#'
#' @examples
n_valid <- function(x){

  sum(!is.na(x))

}
