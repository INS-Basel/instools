#' Overview of color palette proposed by unibas corporate design
#'
#' A table giving an overview all colors (taken from the CI info sheet on ADAM)
#' with both RGB and Hex values
#'
#' @export
#'
#' @examples
#' scales::show_col(unibas_color_palette_overview$hex_value)
unibas_color_palette_overview <-
  tibble::tibble(
        col_num = seq_len(10),
        rgb_value = list(
          list(165, 215, 210),
          list(30, 165, 165),
          list(45, 55, 60),
          list(140, 145, 150),
          list(210, 5, 55),
          list(235, 130, 155),
          list(190, 195, 200),
          list(0, 110, 110),
          list(0, 0, 0),
          list(255, 255, 255)
        )
  ) %>%
  dplyr::mutate(hex_value = purrr::map_chr(rgb_value,
                                           ~grDevices::rgb(.x[1], .x[2], .x[3], maxColorValue = 255)))


#' Colour palette to use in figures
#'
#' A vector holding the information from \code{unibas_color_palette_overview$hex_value}
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(diamonds, aes(x = cut, y = carat)) +
#' geom_col(aes(fill = cut)) +
#' scale_fill_manual(values = unibas_palette) +
#'  labs(x = "",
#'   title = "Diamonds: Carat by Cut",
#'   subtitle = "in the Unibas color-palette") +
#'   theme_minimal()
unibas_palette <- unibas_color_palette_overview$hex_value
