#' Create ranef-dotplot from model
#'
#' @param model a \code{glmer()}-object from class S4
#'
#' @return a plot
#' @export
#'
get_model_dotplot <- function(model){

  lattice::dotplot(lme4::ranef(model, condVar = T))

}



#' ggCaterpillar plot from random effects model
#'
#' @param model glmer-model to extract the random effects out
#' @param condVar input to \code{lme4::ranef(model, condVar)}, default = T
#' @param QQ TRUE/FALSE, default = FALSE
#' @param likeDotplot TRUE/FALSE, default = FALSE
#'
#' @source from [https://stackoverflow.com/questions/13847936/plot-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot-how-to-mak](https://stackoverflow.com/questions/13847936/plot-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot-how-to-mak)
#'
#' @importFrom rlang .data
#'
#' @export
#'
ggCaterpillar <- function(model, condVar = TRUE, QQ = FALSE, likeDotplot = TRUE) {

  re <- lme4::ranef(model, condVar = condVar)

  ID <- ci <- y <- nQQ <- NULL

  f <- function(x) {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
                       ci=1.96*se[ord],
                       nQQ=rep(stats::qnorm(stats::ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))

    if(QQ) {  ## normal QQ-plot
      p <- ggplot2::ggplot(pDf, ggplot2::aes(nQQ, y))
      p <- p + ggplot2::facet_wrap(~ ind, scales="free")
      p <- p + ggplot2::xlab("Standard normal quantiles") + ggplot2::ylab("Random effect quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot2::ggplot(pDf, ggplot2::aes(ID, y)) + ggplot2::coord_flip()
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + ggplot2::facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + ggplot2::facet_grid(ind ~ ., scales="free_y")
      }
      p <- p + ggplot2::xlab("Levels") + ggplot2::ylab("Random effects")
    }

    p <- p + ggplot2::theme(legend.position="none")
    p <- p + ggplot2::geom_hline(yintercept=0)
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
    # p <- p + ggplot2::geom_point(ggplot2::aes(size=1.2), colour="blue")
    p <- p + ggplot2::geom_point(size = 1, colour="blue")
    return(p)
  }

  lapply(re, f)
}


#' create a tidy table out of the glmer-object to plot
#'
#' @param model a \code{glmer()}-object from class S4
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#'
#' @return a tibble with random effects and random coefficients
#' @export
#'
get_model_table <- function(model){

  if (!inherits(model, c('merMod')))
    stop('This is not a supported model class.')

  # specifying newly created variables
  fix_intercept <- ran_intercept <- comb_intercept <- se <- ul <- ll <- NULL
  fix_intercept_p <- comb_intercept_p <- NH_prob_mean <- ul_p <- ll_p <- rating <- NULL
  QI <- ranking_perQI <- center <- grp <- condval <- condsd <- NULL

  # get n per group
  model@frame %>%
    dplyr::count(center) %>%
    dplyr::mutate(center = as.character(center)) -> n_table

  # tibble the random effects
  ranef_model = tibble::as_tibble(lme4::ranef(model, condVar = T))

  # create the table, include the rating
  # all variables ending with _p represent probabilities
  ranef_model =
  ranef_model %>%
    dplyr::mutate(center = as.character(grp),
                  fix_intercept = lme4::fixef(model)[1],
                  ran_intercept = condval,
                  comb_intercept = fix_intercept+condval,
                  se = 1.96*condsd,
                  ul = comb_intercept+se,
                  ll = comb_intercept-se,
                  fix_intercept_p = arm::invlogit(fix_intercept),
                  comb_intercept_p = arm::invlogit(comb_intercept),
                  NH_prob_mean = comb_intercept_p,
                  ul_p = arm::invlogit(comb_intercept+se),
                  ll_p = arm::invlogit(comb_intercept-se),
                  rating = dplyr::case_when(ul_p < fix_intercept_p ~ "sig_lower",
                                            ll_p > fix_intercept_p ~ "sig_higher",
                                            TRUE ~ "average"),
                  rating = forcats::as_factor(rating),
                  rating = forcats::fct_expand(rating, c("sig_lower", "average", "sig_higher")),
                  rating = forcats::fct_relevel(rating, c("sig_lower", "average", "sig_higher")),
                  ranking_perQI = base::rank(NH_prob_mean)) %>%
    dplyr::mutate_if(is.numeric, round, digits = 3)

  # add population size information to the table
  dplyr::left_join(ranef_model, n_table, by = c("center" = "center"))


}
