#' get (proportion) percentage agreement of answers in a vector - 'agreement' to be defined
#' by cutoff
#'
#' @param x a vector
#' @param cutoff a vector defining positive answers, default = 1 in a binary dataset
#' @param restrict_level the minimum number of valid answers needed to display result,
#' otherwise the answer is restricted and set to NA, default is NULL (no restriction)
#'
#' @return
#' @export
#'
#' @examples
prop_agree <- function(x, cutoff = 1, restrict_level = NULL){

  # check nvalid
  nvalid <- n_valid(x)

  # restrict if restrict_level is set
  if (!is.null(restrict_level)) {
    nvalid <- dplyr::case_when(nvalid >= restrict_level ~ nvalid,
                               TRUE ~ NA_integer_)
    }



  # calculate result with restricted nvalid
  result <- sum(x %in% cutoff, na.rm = T)/nvalid



  return(result)

}
