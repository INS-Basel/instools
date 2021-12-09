#' Create a customized stacked barplot with percentage-frequencies
#'
#' @param data a data frame as result of answ_freq_table
#' @param vars a vector passing a selection of variables
#' @param expand_to specify all factor levels (numeric), arguments are passed to fct_expand
#' @param relevel_by specify order of factor levels, arguments passed to fct_relevel
#' @param recode_to_german specify (alphanumeric) answer options, arguments passed to fct_recode, example: c("Neuer Text" = "1", "Zweiter Text" = "2")
#' @param recode_to_french specify (alphanumeric) answer options, arguments passed to fct_recode
#' @param recode_to_italian specify (alphanumeric) answer options, arguments passed to fct_recode
#' @param set_colors specify coloring by (numeric) answer, arguments passed to fct_recode
#' @param language one of c("german, "french", "italian"), default == "german", abbreviations tolerated
#'
#' @keywords internal
#'
#' @return a ggplot-object
#'
ggStackfreq <- function(data,
                        vars,
                        expand_to,
                        relevel_by,
                        recode_to_german,
                        recode_to_french,
                        recode_to_italian,
                        set_colors,
                        language){


  # create the frequency table
  data <- answ_freq_table(data,
                          vars = vars,
                          expand_to = expand_to,
                          relevel_by = relevel_by,
                          recode_to_german = recode_to_german,
                          recode_to_french = recode_to_french,
                          recode_to_italian = recode_to_italian,
                          set_colors = set_colors)


  #define global variables
  n <- percent <- text_cumsum <- text_pos <- unit <- value <- variable <- language <- NULL

  # arrange the dataframe to calculate the position of the labels
  data <-
    data %>%
    dplyr::arrange(variable, value) %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(text_cumsum = base::cumsum(percent),
                  text_pos = text_cumsum-(percent/2),
                  N = base::sum(n)) %>%
    dplyr::ungroup()

  answer_labels <- base::levels(data$answer_fct)

  ###################### setting answer labels ######################
  # language dependent labelling

  # if(language %in% c("german", "g")) {
  #
  # answer_labels <-
  #   base::levels(data$answer_opt_g) } else if (language %in% c("french", "f")){
  #
  #     answer_labels <-
  #       base::levels(data$answer_opt_f) } else if (language %in% c("italian", "i")){
  #
  #         answer_labels <-
  #           base::levels(data$answer_opt_i)
  #
  #       }

  ###################### setting labels for colours ######################

  # check hex color codes by
  # scales::show_col("#fef0d9")


  ############### for now - take the colouring from the ground table ############
  colours <- base::levels(data$answer_color)

  # scales::show_col(colors4)

  # Create the ggplot
  base_plot <-
    ggplot2::ggplot(data, ggplot2::aes(x = forcats::fct_rev(variable),
                                       y = percent,
                                       fill = value)) +
    ggplot2::geom_col(width = .7) +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                breaks = seq(0, 1, .1),
                                name = NULL)


  # add customized colours
  if(!is.null(colours)){

    base_plot <-
      base_plot +
      ggplot2::scale_fill_manual(values = colours,
                                 name = NULL,
                                 drop = FALSE,
                                 labels = stringr::str_wrap(answer_labels, 18),
                                 guide = ggplot2::guide_legend(reverse = FALSE))
  }

  # add coord_flip and theme
  base_plot <-
    base_plot +
      ggplot2::coord_flip() +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "bottom",
                     legend.direction = "horizontal",
                     legend.key.size = ggplot2::unit(1,"line"))

  # add labels
  base_plot +
    ggrepel::geom_label_repel(ggplot2::aes(y = 1-text_pos,
                                           label = scales::percent(percent, .1, 100)),
                              nudge_y      = 0.01,
                              nudge_x      = -0.1,
                              direction    = "x",
                              angle        = 0,
                              vjust        = 0,
                              segment.size = 0.1,
                              label.size   = 0.3,
                              size         = 3.2,
                              show.legend  = FALSE)


}
