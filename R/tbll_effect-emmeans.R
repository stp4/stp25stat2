#' @description  Effect Displays
#'
#' @param include.ci an broom::tidy mit conf.int = include.ci, conf.level = 0.95,
#' @param ... an  broom::tidy zB  adjust = "tukey"
#'
#' @rdname extract
#' @export
#'
#' @examples
#'
#'  \donttest{
#' require(emmeans)
#'
#' warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
#' x1 <- emmeans(warp.lm, ~ wool | tension)
#' Tbll(emmeans(warp.lm, ~ wool | tension))
#'
#' x2 <- emmeans(warp.lm, poly ~ tension | wool)
#' Tbll(emmeans(warp.lm, poly ~ tension | wool))
#'
#' x3 <- emmeans(warp.lm, pairwise ~ tension | wool)
#' Tbll(emmeans(warp.lm, pairwise ~ tension | wool))
#'
#' Tbll(x3, adjust = "tukey")
#' Tbll(x3, adjust = "scheffe")
#'
#' # ?summary.emmGrid
#'
#'
#' }
tbll_extract.emm_list <-
  function(x,
           include.ci = TRUE,
           ...) {
    means <-
      broom::tidy(x$emmeans,
                  conf.int = include.ci,
                  conf.level = 0.95,
                  ...)
    contrasts <-
      broom::tidy(x$contrasts,
                  conf.int = include.ci,
                  conf.level = 0.95,
                  ...)

    list(
      means = prepare_output(fix_format(means),
                             caption = "Means"),
      contrasts = prepare_output(fix_format(contrasts),
                                 caption = "Contrasts",)
    )

  }


#' @rdname extract
#' @export
tbll_extract.emmGrid <-
  function(x,
           include.ci = TRUE,
           ...) {
    contrasts <-  means <- NULL
    if (is.null(names(x))) {
      means <-
        broom::tidy(x,
                    conf.int = include.ci,
                    conf.level = 0.95,
                    ...)

      prepare_output(fix_format(means),
                     caption = "Means")

    }
    else{
      means <-
        broom::tidy(x$emmeans,
                    conf.int = include.ci,
                    conf.level = 0.95,
                    ...)

      contrasts <-
        broom::tidy(x$contrasts,
                    conf.int = include.ci,
                    conf.level = 0.95,
                    ...)

      list(
        means = prepare_output(
          fix_format(means),
          caption = "Means"
        ),
        contrasts = prepare_output(
          fix_format(contrasts),
          caption = "Contrasts"
        )
      )
    }
  }


#' @rdname extract
#' @export
tbll_extract.visreg <- function(x,
                                include.ci = TRUE,
                                digits = 2,
                                ...) {
  caption <- x$meta$y
  res <- x$fit
  nc <- ncol(res)
  ci <- res[, (nc - 1):nc]

  res <- res[,-c((nc - 1):nc)]
  res[, nc - 2] <-
    render_f(res[, nc - 2],  digits = digits)
  names(res)[nc - 2] <- "fit"

  if (include.ci)
    res$ci <- rndr_CI(ci,  digits = digits)

  prepare_output(res[-which(names(res) == x$meta$y)],
                 caption = caption,
                 N = nrow(x$res))
}




