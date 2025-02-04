#' Estimate Marginal Contrasts
#'
#' Stolen from modelbased::estimate_contrasts()
#'
#' @param x A statistical model. or class estimate_contrasts
#' @param contrast If the contrasts are not null, the contrasts are passed to the estimate_contrasts() function (character vector containing the name of the variable(s)).
#' If null, the name of the variable is extracted and the contrasts are evaluated separately.
#' @param p_adjust  Can be one of "holm" (default), "tukey", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr" or "none". See the orginal estimate_contrasts()
#' @param method Contrast method. "pairwise"
#' @param digits only for Coefficient and CIs
#' @param include.label not working
#' @param ... Other arguments passed to estimate_contrasts() by = modulate it
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' fit <- lm(breaks ~ wool * tension, data = warpbreaks)
#'
#' modelbased::estimate_contrasts(fit, contrast =  "tension") |> Tbll_contrasts()
#' Tbll_contrasts(fit, contrast =  "tension")
#' Tbll_contrasts(fit)
#' # require(effects)
#' # # Results may be misleading due to involvement in interactions
#' # mod <- lm(prestige ~ type * (education + income) + women, Prestige)
#' # Tbll_contrasts(mod)
#'
Tbll_contrasts  <- function(x,
                            contrast =  NULL,
                            p_adjust = "holm",
                            method = "pairwise",
                            digits = 2,
                            include.label = FALSE,
                            ...) {
  if (!inherits(x, "estimate_contrasts")) {
    if (is.null(contrast)) {
      contrast <-
        names(which(sapply(
          insight::get_predictors(x), is.factor
        )))
      cat("\nContrast:", contrast, "\n\n")
      rslt <- list()
      for (i in contrast) {
        rslt[[i]] <-
          Tbll_contrasts(
            x,
            contrast =  i,
            p_adjust = p_adjust,
            method = method ,
            digits = digits,
            include.label = include.label,
            ...
          )
      }
    }
    else {
      rslt <-
        modelbased::estimate_contrasts(x,
                                       contrast = contrast,
                                       p_adjust = p_adjust,
                                       method = method)
      rslt <- format_estimate_contrasts(rslt, digits = digits)
    }
  }
  else{
    rslt <- format_estimate_contrasts(x, digits = digits)
  }
  rslt
}

#' @noRd
format_estimate_contrasts <-
  function(x, digits = 2) {
    x |>
      insight::standardize_names() |>
      fix_format_insight(digits = digits) |>
      prepare_output(caption =  paste0(attr(x, "table_title")[1],
                                       ". Response: ",
                                       attr(x, "response")),
                     note = gsub( "\n", " ", attr(x, "table_footer")[1])
      )

  }


#' @noRd
fix_format_insight <-
  function(x, digits = 2) {
    x$Statistic <- render_f(x$Statistic, digits)
    x$df <- as.character(x$df)
    x$p <- rndr_P(x$p, include.symbol = FALSE)

    purrr::map2_dfc(x, digits, function(x, digits) {
      if (is.numeric(digits))
        render_f(x, digits)
      else
        x
    })
  }
