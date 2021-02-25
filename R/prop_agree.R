#' get (proportion) percentage agreement of answers in a vector - 'agreement' to be defined
#' by cutoff
#'
#' @param x a numeric vector
#' @param cutoff a vector defining positive answers, default = 1 in a binary dataset
#' @param restrict_level the minimum number of valid answers needed to display result,
#' otherwise the answer is restricted and set to NA, default is NULL (no restriction)
#'
#' @return a numeric value between 0 - 1
#' @export
#'
#' @examples
#' # binary vector
#' vec <- c(0, 1, 1, 0, 0, 1, NA_real_, 1)
#' prop_agree(x = vec, cutoff = 1)
prop_agree <- function(x, cutoff = 1, restrict_level = NULL){

  # asserthat assumption
  assertthat::assert_that(base::is.numeric(x))

  # check nvalid
  nvalid <- n_valid(x)

  # restrict if restrict_level is set
  if (!is.null(restrict_level)) {
    nvalid <- dplyr::case_when(nvalid >= restrict_level ~ nvalid,
                               TRUE ~ NA_integer_)
    }

  # calculate result
  result <- sum(x %in% cutoff, na.rm = T)/nvalid

  # define return
  return(result)

}
