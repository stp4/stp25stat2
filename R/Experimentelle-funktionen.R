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
#' @export
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




#' Anova
#'
#'  Anova from car::Anova
#'
#' @param ...  lm-Objects
#' @param names  Response
#' @param include.eta an tbll_extract
#' @param type an car::Anova
#'
#' @return tibble, data.frame
#' @export
#'
#' @examples
#'
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.D9 <- lm(weight ~ group)
#' lm.D90 <- lm(weight ~ group - 1) # omitting intercept
#'
#'
#' Tbll_anova(lm.D9, lm.D90)
#'
#'
Tbll_anova <- function(...,
                       names = NULL,
                       include.eta = TRUE,
                       type=c("II","III", 2, 3)) {
  dots <- list(...)

  if (is.null(names)) {
    names <-
      gsub("[~??+\\:=]", "",
           as.character(as.list(sys.call()))[seq_len(length(dots)) + 1])
  }

  if (length(dots) == 1)
    tbll_extract(car::Anova(dots[[1]], type=type),
                 include.eta = include.eta)
  else{
    rslt <-  NULL
    for (i in  seq_along(dots)) {
      rst <-   cbind(Response = "",
                     tbll_extract(car::Anova(dots[[i]], type = type),
                                  include.eta = include.eta))
      rst[1, 1] <- names[i]
      rslt <-  rbind(rslt, rst)

    }
    rslt
  }
}
