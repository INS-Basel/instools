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
#' # sample three different dfs
#' df1 <- tibble::tibble(ID = seq_len(100), var1 = sample(c(1:3), 100, TRUE))
#' df2 <- tibble::tibble(ID = seq_len(300), other_var2 = sample(c(letters[1:5]), 300, TRUE))
#' df3 <- tibble::tibble(ID = seq_len(5600), other_var3 = sample(c(letters[17:23]), 5600, TRUE))
#'
#' # check with one dataframe
#' file_created_on(df1)
#'
#' # list the dataframes
#' # map (apply) the function to each dataframe
#' # and write it back to the GlobalEnvironment
#' \dontrun{tibble::lst(df1, df2, df3) %>%
#'  purrr::map(~file_created_on(.x)) %>%
#'   base::list2env(., envir = .GlobalEnv)}
file_created_on <- function(.data,
                            var_name = "file_created_on"){

  # add variable with today's date
  dplyr::mutate(.data = .data,
                !!var_name := lubridate::today())

}
