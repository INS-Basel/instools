#' unibas ggplot theme
#'
#' [ggplot2] plot theme based on colors of UNIBAS
#'
#' @param base_theme A base theme upon which additional theme-specific options are applied
#' @param base_size Base font size, defaults to 12, passed to theme
#' @param ... Further arguments passed to \code{ggplot2::theme()}
#'
#' @seealso [\code{ggplot2::theme()}](https://ggplot2.tidyverse.org/reference/theme.html)
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = as.factor(1:8),
#'                  y = c(300, 500, 600, 750, 700, 550, 450, 350))
#'
#' ggplot(df, aes(x, y, fill = x)) +
#'   geom_col() +
#'   scale_fill_unibas_d() +
#'   theme_unibas()
#'
#' @export
theme_unibas <- function(base_theme = ggplot2::theme_minimal(),
                         base_size = 12,
                         ...){

  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(family = "Georgia"),
      plot.title.position = "plot",
      ...)
}
