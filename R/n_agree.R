#' get number of agreement answers (defined by a cutoff) in a vector
#'
#' @param x a numeric/integer vector
#' @param cutoff a vector defining positive answers, default = 1 in a binary dataset
#'
#' @return a numeric value
#' @export
#'
#' @examples
#' # usage with dichotomized vector
#' vec <- c(0, 0, 1, 1, 1)
#' n_agree(vec)
#'
#' # usage with non-dichotomized vector:
#' vec <- c(1, 2, 3, 4, 3, 3, 4, 3, 4, 4, NA)
#' # you need to change the cutoff
#' n_agree(vec, cutoff = c(3,4))
#'
#'
#'
n_agree <- function(x, cutoff = 1){

  # check input
  assertthat::assert_that(base::is.numeric(x))

  as.integer(sum(x %in% cutoff, na.rm = T))

}
