#' Format numbers according to journal requirements
#'
#' @param x an integer
#' @param n_break fix number where the formatting sets the break
#' @param ... additional arguments passed to base::format()
#'
#' @return an formatted character string
#' @keywords internal
journal_format <- function(x, n_break = 4, ...){
  dplyr::case_when(
    base::nchar(x) > n_break ~ base::format(x, ...),
    TRUE ~ base::format(x))
}
