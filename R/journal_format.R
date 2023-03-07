#' Format numbers according to journal requirements
#'
#' @details some journal require to format big numbers only from the
#' fifth digit (instead of the fourth) - this functions fixes this
#'
#' @param x an integer-vector
#' @param n_break fix number where the formatting sets the break
#' @param big.mark defaults to ",", passed to \code{base::format(big.mark)}
#' @param ... additional arguments passed to \code{base::format()}
#'
#' @return an formatted character string
#' @export
#'
#' @examples
#' vec <- c(1L, 20L, 300L, 4000L, 50000L)
#' journal_format(vec, big.mark = ",")
journal_format <- function(x, n_break = 4, big.mark = ",", ...){

  # make sure that we have either numeric or integer
  checkmate::assert_integerish(x)

  dplyr::case_when(
    base::nchar(x) > n_break ~ base::format(x, big.mark = big.mark, ...),
    TRUE ~ base::format(x))
}
