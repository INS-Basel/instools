#' Adds 'file_created_on'-variable to a tidy dataframe
#'
#'
#' @param .data a tidy dataframe
#' @param var_name name of the variable to be added, defaults to "file_created_on"
#'
#' @return a tidy dataframe
#' @export
#'
#' @importFrom rlang .data :=
#'
#' @examples
#'
#' library(magrittr)
#' # sample dataframe
#' df1 <- tibble::tibble(ID = seq_len(100), var1 = sample(c(1:3), 100, TRUE))
#'
#' # add variable
#' file_created_on(df1)
file_created_on <- function(.data,
                            var_name = "file_created_on"){

  # add variable with today's date
  dplyr::mutate(.data = .data,
                !!var_name := lubridate::today())

}
