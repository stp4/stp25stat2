#' pairwise.wilcox.test
#'
#' Post-hoc-Tests sind eine Familie von statistischen Tests, von denen es mehrere gibt.
#'
#'   Tukey HSD, wird verwendet, um alle Gruppen miteinander zu vergleichen (also alle möglichen Vergleiche von 2 Gruppen).
#'
#'   Dunnett, um Vergleiche mit einer Referenzgruppe (Kontrollgruppe) durchzuführen.
#'
#'   Bonferroni-Korrektur, wenn eine Reihe von geplanten Vergleichen durchgeführt werden soll.
#'
#' @param ...  data, formula, names
#' @param p.adjust.method method for adjusting p value c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none")
#' @param alternative,mu,paired,exact,correct,conf.int,conf.level,tol.root,digits.rank not used
#'
#' @return data.frame
#'
#' @examples
#' #' require(stp25stat2)
#' airquality$Month <- factor(airquality$Month)
#' Post_Hoc(airquality, Ozone, Month)
#' Post_Hoc(airquality, Ozone, Month, test= "t.test")
#' res_aov<-aov( Ozone ~ Month, airquality)
#' TukeyHSD( res_aov ) %>% Tbll()
#' DescTools::PostHocTest( res_aov, method = "hsd")
#' # Dunnett's test:
#' library(multcomp)
#' post_test <-
#'   glht(res_aov,
#'        linfct = mcp(Month = "Dunnett"))
#'
#' summary(post_test)
#' summary(lm(res_aov))
Post_Hoc <- function(...,
                     p.adjust.method = "bonf",
                     # not used
                     # additional arguments to pass to wilcox.test.
                     test = "wilcox.test",
                     alternative = c("two.sided", "less", "greater"),
                     mu = 0,
                     paired = FALSE,
                     exact = NULL,
                     correct = TRUE,
                     conf.int = FALSE,
                     conf.level = 0.95,
                     tol.root = 1e-4,
                     digits.rank = Inf) {
  X <- stp25tools::prepare_data2(...)

  if( test == "wilcox.test"){
    cat("\npairwise.wilcox.test\n")
    suppressWarnings(
  rslt <-
    pairwise.wilcox.test(X$data[[1]],
                         X$data[[2]],
                         p.adjust.method = p.adjust.method))
  }
  else if ( test == "t.test"){
    cat("\npairwise.t.test\n")
    rslt <-
      pairwise.t.test(X$data[[1]],
                           X$data[[2]],
                           p.adjust.method = p.adjust.method)
  }

  tbll_extract(rslt,
               caption =
                 paste(paste(X$row_name, collapse = " by "),
                       rslt$method,
                       " (p-value)"
                       ))

}

# require(stp25stat2)
#  airquality$Month <- factor(airquality$Month)
#  Post_Hoc(airquality, Ozone, Month)
#  Post_Hoc(airquality, Ozone, Month, test= "t.test")
#  res_aov<-aov( Ozone ~ Month, airquality)
#  TukeyHSD( res_aov ) %>% Tbll()
#  DescTools::PostHocTest( res_aov, method = "hsd")
#   # Dunnett's test:
#  library(multcomp)
#   post_test <-
#     glht(res_aov,
#                    linfct = mcp(Month = "Dunnett"))
#
#  summary(post_test)
#  summary(lm(res_aov))


