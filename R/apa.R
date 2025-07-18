#' Text-Ausgabe von Tests
#'
#' Ausgabe von APA-Style formatiertem Text.
#'
#'
#' @name APA
#' @param x Objekt fit, formula usw
#' @param ...  digits, data, usw
#' @return Character Vector mit einem oder meheren Eintraegen
#' @export
#' @examples
#'
#'   \donttest{
#' APA(mpg ~ cyl, mtcars)
#' APA(glm(vs ~ mpg, mtcars, family = binomial()))
#' APA(lm(mpg ~ drat + wt + qsec, mtcars))
#' APA(aov(mpg ~ drat + wt + qsec, mtcars))
#' APA(wilcox.test(mpg ~ vs, mtcars))
#' APA(t.test(mpg ~ vs, mtcars))
#'
#' a <- letters[1:3]
#' APA(summary(table(a, sample(a))))
#'
#' TeaTasting <-
#'   matrix(c(3, 1, 1, 3),
#'          nrow = 2,
#'          dimnames = list(
#'            Guess = c("Milk", "Tea"),
#'            Truth = c("Milk", "Tea")
#'          ))
#' APA(fisher.test(TeaTasting, alternative = "greater"))
#'
#' # Test Survival Curve Differences
#' require(survival)
#' APA(survdiff(Surv(futime, fustat) ~ ecog.ps, data = ovarian))
#'
#'
#'
#' require(coin)
#' davis <- matrix(c(3, 6, 2, 19),
#'                 nrow = 2, byrow = TRUE)
#' davis <- as.table(davis)
#' ## Asymptotic Pearson chi-squared test
#' diffusion <- data.frame(
#'   pd = c(
#'     0.80,    0.83,    1.89,    1.04,    1.45,
#'     1.38,    1.91,    1.64,    0.73,    1.46,
#'     1.15,    0.88,    0.90,    0.74,    1.21),
#'   age = factor(rep(c( "At term", "12-26 Weeks"), c(10, 5)))
#' )
#'
#' ## Exact Wilcoxon-Mann-Whitney test
#' ## Hollander and Wolfe (1999, p. 111)
#' ## (At term - 12-26 Weeks)
#' wt <- wilcox_test(pd ~ age,
#'                   data = diffusion,
#'                   distribution = "exact",
#'                   conf.int = TRUE)
#'
#' APA(xt <- chisq_test(davis))
#' APA(wt)
#' }
APA <-   function(x,
                  ...) {
  UseMethod("APA")
}

#' @rdname APA
#' @export
APA.psych  <- function(x, ...) {
  paste("Alpha =", render_f(x$total$raw_alpha, digits = 2))
}


#' @rdname APA
#' @export
APA.psych_alpha <- function(x, ...) {
  paste("Alpha =", x$scale_statistics$Alpha)
}



#' @rdname APA
#' @export
APA.coxph <-
  function (x,
            ...) {
    gmodel <- broom::glance(x)
    paste0(
      "# Events: ",
      gmodel$nevent,
      "; Global p-value (Log-Rank): ",
      rndr_P(gmodel$p.value.log),
      " \nAIC: ",
      round(gmodel$AIC, 0),
      "; Concordance Index: ",
      round(gmodel$concordance, 2)
    )

  }


#' @rdname APA
#' @export
APA.summary.table <- function(x,
                              ...) {
  paste0("Chisq(df=",
         x$parameter,
         ")=",
         render_f(x$statistic, 2),
         ", ",
         rndr_P(x$p.value))
}

#' @rdname APA
#' @export
APA.survdiff <- function(x,
                         ...) {
  df <- length(x$n) - 1
  p.val <- 1 - pchisq(x$chisq, df)
  paste0("Log Rank-Test ",
         rndr_X(x$chisq, df),
         ", ",
         rndr_P(p.val))
}

#' @rdname APA
#' @export
APA.NULL <- function(x, ...) {
  # res<- Info_Statistic(
  #   c("catTest", "conTest", "Wilkox", "Kruskal",
  #     "ANOVA",
  #     "T Test"),
  #   c("stats", "Hmisc", "stats", "stats",
  #     "car",
  #     "stats"),
  #   c(
  #     "chisq.test",
  #     "spearman2",
  #     "wilcox.test",
  #     "kruskal.test",
  #     "Anova, type = 3",
  #     "t.test"
  #   ), paste(methods("APA"), collapse=", ")
  # )

  "Tests siehe APA"
}

#' @rdname APA
#' @export
APA.default <- function(x,
                        ...) {
  cat("\nKeine Methode fuer: ", class(x), "\n")
  class(x)[1]
}

#' @rdname APA
#' @export
APA.QuadTypeIndependenceTest <- function(x,
                                         ...) {
  # capture.output(x)[5]
  rndr_Chisq(coin::statistic(x),
             x@statistic@df,
             coin::pvalue(x))
}

#' @rdname APA
#' @export
APA.ScalarIndependenceTest <- function(x,
                                       ...) {
  rndr_W(coin::statistic(x), coin::pvalue(x))
}

#' @rdname APA
#' @export
APA.numeric <- function(x, ...)
  calc_mean(x, ...)

# @rdname APA
# @export
#APA.factor <- function(x,...) Prozent(...)

#' @rdname APA
#' @param data,exclude,max_factor_length bei der Verwendung von Formeln
#' @export
APA.formula <- function(x,
                        data,
                        exclude = NA,
                        max_factor_length = 25,
                        max_value =FALSE,
                        ...) {
  X <- stp25tools::prepare_data2(x, data)
  res <- NULL
  if (is.null(X$group.vars)) {
    for (i in seq_along(X$measure)) {
      x <- X$data[[X$measure.vars[i]]]
      x    <- na.omit(x)
      n    <- length(x)

      if (all(is.na(x)))
        X$measure[i] <- "all_NA"

      res1 <- switch(
        X$measure[i],
        numeric = mean_tbll(x, X$digits[i], n),
        integer = mean_tbll(x, X$digits[i], n),
        factor =  prct_tbll(x, X$digits[i], n, exclude, max_factor_length),
        logical = prct_tbll(x, X$digits[i], n, exclude, max_factor_length),
        freq =    prct_tbll(x, X$digits[i], n, exclude, max_factor_length),
        mean =    mean_tbll(x, X$digits[i], n),
        median =   median_tbll(x, X$digits[i], n),
        multi =    multi_tbll(x, X$digits[i], n),

        c(lev = "NA", n = "NA", m = "NA")
      )
      m <- as.character(res1$m)
     # names(m) <- X$measure.vars[i]

      names(m) <- gsub(", ", "", res1$lev)

      if(max_value) {
        max_value <- switch(
          X$measure[i],
          factor = which.max( table(x) ),
          freq = which.max(table(x)),
          1
        )
        m <- m[max_value]
      }
      res <- c(res, m)
    }
  }
  else{
    # cat("\ngroups\n")
    if (length(X$measure) != 1)
      stop("Nur eine Measure-Variable ist erlaubt!")

    if (length(X$group.vars) > 1) {
      X$data$group <- interaction2(X$data[X$group.vars])
      caption <- paste(X$group.vars, collapse = ", ")
      X$group.vars <- "group"
    } else {
      caption <-  X$group.vars
    }

    data <- split(X$data[X$measure.vars], X$data[[X$group.vars]])
    # return(data)
    for (i in seq_along(levels(X$data[[X$group.vars]]))) {
      x <- data[[i]][[1]]
      x    <- na.omit(x)
      n    <- length(x)

      if (all(is.na(x)))
        X$measure[i] <- "all_NA"

      res1 <- switch(
        X$measure[1],
        numeric = mean_tbll(x, X$digits[1], n),
        integer = mean_tbll(x, X$digits[1], n),
        factor =  prct_tbll(x, X$digits[1], n, exclude, max_factor_length),
        logical = prct_tbll(x, X$digits[1], n, exclude, max_factor_length),
        freq =    prct_tbll(x, X$digits[1], n, exclude, max_factor_length),
        mean =    mean_tbll(x, X$digits[1], n),
        median =   median_tbll(x, X$digits[1], n),
        multi =    multi_tbll(x, X$digits[1], n),

        c(lev = "NA", n = "NA", m = "NA")
      )

      # print(res1)
      m <- as.character(res1$m)
      names(m) <- names(data)[i]

      res <- c(res, m)

    }

  }
  res
}


#' @rdname APA
#' @param include.r APA.lm: R-Squar
#' @export
APA.lm <- function(x,
                   include.r = TRUE,
                   ...) {
  if (any(class(x) == "aov"))
    x <- lm(x)
  fitSummary <- summary(x)
  fstats <- fitSummary$fstatistic
  pValue <-  stats::pf(fstats[['value']],
                       fstats[['numdf']],
                       fstats[['dendf']], lower.tail = FALSE)
  if (include.r)
    rndr_lm(fstats[['value']] ,
            fstats[['numdf']],
            fstats[['dendf']],
            pValue,
            fitSummary$r.squared,
            fitSummary$adj.r.squared)
  else
    rndr_F(fstats[['value']] ,
           fstats[['numdf']],
           fstats[['dendf']],
           pValue)

}


#' @rdname APA
#' @export
APA.glm <- function(x,
                    ...) {
  # Hier gibt es Probleme wen die Funktion in
  # anderen verschachtelt ist
  # lrtst <-  lmtest::lrtest(x)
  # paste0("LogLik=",
  #        Format2(lrtst[2, 2], 2),
  #        ", ",
  #        rndr_X(lrtst[2, 4],
  #               lrtst[1, 1],
  #               lrtst[2, 1],
  #               lrtst[2, 5]))
  #
  #
  # rhs <-  formula(x)[-2]
  # fm0 <-
  #   as.formula(paste(".~ -", paste(all.vars(rhs), collapse = " - ")))
  # null_model <- update(x, fm0)
  #
  lhs <-  formula(x)[[2]]
  null_model <-
    glm(as.formula(paste(lhs, "~1")),
        data =  x$data,
        family = x$family)

  ll_fit <- logLik(x)
  ll_0 <- logLik(null_model)

  chi2 <- -2 * (as.numeric(ll_0) -  as.numeric(ll_fit))
  df_fit <-  df <- attr(ll_fit, "df") - 1

  paste0("LogLik=",
         render_f(as.numeric(ll_fit), 2),
         ", ",
         rndr_X(
           chi2,
           df1 = df_fit,
           p = pchisq(chi2, df = df_fit, lower.tail = FALSE)
         ))
}


#' @rdname APA
#' @export
APA.htest <- function(x,
                      ...) {
  if (any(names(x) == "statistic")) {
    if (names(x$statistic) == "t") {
      rndr_T(x$statistic,
             x$parameter,
             x$p.value)
    }
    else if (names(x$statistic) == "BP") {
      rndr_BP(x$statistic,
              x$parameter,
              x$p.value)
    }
    else if (names(x$statistic) == "DW") {
      rndr_DW(x$statistic,
              x$parameter,
              x$p.value)
    }
    else if (names(x$statistic) == "X-squared") {
      rndr_Chisq(x$statistic, x$parameter, x$p.value)
    }
    else{
      rndr_W(x$statistic,
             x$p.value)
    }
  }
  else{
    rndr_fischer(x$estimate, x$p.value)
  }

}


#' @rdname APA
#' @export
APA.xtabs <- function(x,
                      ...) {
  x <- summary(x)
  rndr_Chisq(x$statistic, x$parameter, x$p.value)
}


#' @rdname APA
#' @export
APA.table <- function(x,
                      ...)
  APA.xtabs(x, ...)


#' @rdname APA
#' @export
APA.summary.table <- function(x,
                              ...) {
  paste0("Chisq(df=",
         x$parameter,
         ")=",
         render_f(x$statistic, 2),
         ", ",
         rndr_P(x$p.value))
}








# Experimental
rndr_test  <-
  function (x,
            df1 = NULL,
            df2 = NULL,
            p = NULL,
            symbol = "F",
            digits = get_opt("Fstat", "digits") ,
            drop0leading = !get_opt("Fstat", "lead.zero")) {
    input <- c(!is.null(df1) , !is.null(df2), !is.null(p))
    if (all(input))
      paste(
        symbol,
        "(",
        formatC(df1, format = "f", digits = 0),
        ",",
        formatC(df, format = "f", digits = 1),
        ") = ",
        formatC(x, format = "f", digits = digits),
        ", ",
        rndr_P(p, symbol.leading = c("p = ", "p < "),),
        sep = ""
      )
    else  if (sum(input) == 2)
      paste(
        symbol,
        "(",
        formatC(df1, format = "f", digits = 0),
        ") = ",
        formatC(x, format = "f", digits = digits),
        ", ",
        rndr_P(if (is.null(p))
          df2
          else
            p,  symbol.leading = c("p = ", "p < "),),
        sep = ""
      )
    else
      paste(symbol, " = ", formatC(x, format = "f", digits = digits),
            sep =
              "")

  }
