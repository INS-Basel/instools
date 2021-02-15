#' get number of agreement answers (defined by cutoff) in a vector
#'
#' @param x a vector
#' @param cutoff a vector defining positive answers, defaults = 1 in a binary dataset
#'
#' @return a integer value
#' @export
#'
#' @examples
#' vec <- c(1, 2, 3, 4, 3, 3, 4, 3, 4, 4, NA)
#' # this is not a dichotomized vector, therefore you need
#' # to change the cutoff
#' n_agree(vec, cutoff = c(3,4))
#'
n_agree <- function(x, cutoff = 1){

  sum(x %in% cutoff, na.rm = T)

}
