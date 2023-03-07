#' Unibas colour palette to use in figures
#'
#' @name unibas_pal
#' @export
unibas_pal<- c(
  "mint" = "#A5D7D2",
  "mintdark" = "#1EA5A5",
  "greydark" = "#2D373C",
  "grey" = "#8C9196",
  "reddark" = "#D20537",
  "greylight" = "#EB829B",
  "greylight" = "#BEC3C8",
  "tuerkis" = "#006E6E",
  "black" = "#000000",
  "white" = "#FFFFFF")

#' @name unibas_pal
#' @export
# Define opinionated discrete palette (good, neutral, bad)
unibas_pal_op <- c(unibas_pal[1], unibas_pal[7], unibas_pal[6])

#' @name unibas_pal
#' @export
# Define two colors for endpoints of continuous palette
unibas_pal_cont <- c(unibas_pal[1], unibas_pal[2])

#' @name unibas_pal
#' @export
# Define three colors for endpoints of diverging continuous palette (high, middle, low)
unibas_pal_div  <- c(unibas_pal[1], unibas_pal[2], unibas_pal[4])
#
#
