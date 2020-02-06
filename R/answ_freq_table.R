#' Create a frequency table of answers options
#'
#' @param data a dataframe
#' @param vars a vector passing a selection of variables
#' @param expand_to specify all factor levels (numeric), arguments are passed to fct_expand
#' @param relevel_by specify order of factor levels, arguments passed to fct_relevel
#' @param recode_to_german specify (alphanumeric) answer options, arguments passed to fct_recode,
#' example: c("Neuer Text" = "1", "Zweiter Text" = "2")
#' @param recode_to_french specify (alphanumeric) answer options, arguments passed to fct_recode
#' @param recode_to_italian specify (alphanumeric) answer options, arguments passed to fct_recode
#' @param set_colors specify coloring by (numeric) answer, arguments passed to fct_recode
#'
#
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @return a dataframe
#' @export
#'
answ_freq_table <- function(data,
                            vars,
                            expand_to,
                            relevel_by,
                            recode_to_german,
                            recode_to_french,
                            recode_to_italian,
                            set_colors){

  # define global variables
  value <- answer_fct <- NULL

  # create a list to map over
  my_list <-
    data %>%
     dplyr::select(tidyselect::all_of({{vars}})) %>% base::as.list()


  # create the base table
  base_table <-
    purrr::map_dfr(my_list, .f = { ~forcats::as_factor(.x) %>% janitor::tabyl(.x,
                                                                              show_na = FALSE,
                                                                              show_missing_levels = TRUE)},
                   .id = "variable") %>%
    dplyr::rename("value" = ".") %>%
    dplyr::mutate(value = forcats::as_factor(value),
                  answer_fct = forcats::as_factor(value),
                  answer_opt_g = NA_character_,
                  answer_opt_f = NA_character_,
                  answer_opt_i = NA_character_,
                  answer_color = NA_character_) %>%
    dplyr::mutate_at(.vars = dplyr::vars(value, answer_fct),
                     .funs = ~forcats::fct_inseq(.))

  # add all the optionals - expand factor levels
  if(!missing(expand_to)){

    base_table <-
      base_table %>%
      dplyr::mutate_at(.vars = dplyr::vars(value, answer_fct),
                       .funs = ~forcats::fct_expand(expand_to)) %>%
      dplyr::mutate_at(.vars = dplyr::vars(value, answer_fct),
                       .funs = ~forcats::fct_inseq(.))
  }

  # add all the optionals - relevel the fct levels
  if(!missing(relevel_by)){

    base_table <-
      base_table %>%
      dplyr::mutate_at(.vars = dplyr::vars(value, answer_fct),
                       .funs = ~fct_relevel(!!!relevel_by))
  }


  # add all the optionals - expand recode answer options
  if(!missing(recode_to_german)){

    base_table <-
      base_table %>%
      dplyr::mutate(answer_opt_g = forcats::fct_recode(answer_fct, !!!recode_to_german))

  }

  # add all the optionals - expand recode answer options
  if(!missing(recode_to_french)){

    base_table <-
      base_table %>%
      dplyr::mutate(answer_opt_f = forcats::fct_recode(answer_fct, !!!recode_to_french))

  }

  # add all the optionals - expand recode answer options
  if(!missing(recode_to_italian)){

    base_table <-
      base_table %>%
      dplyr::mutate(answer_opt_i = forcats::fct_recode(answer_fct, !!!recode_to_italian))

  }

  # add all the optionals - set colors for plotting
  if(!missing(set_colors)){

    base_table <-
      base_table %>%
      dplyr::mutate(answer_color = forcats::fct_recode(answer_fct, !!!set_colors))

  }

  base_table <- tibble::as_tibble(base_table)

  #specify the output
  return(base_table)

  # return(base_table)

}
