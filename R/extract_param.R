#' @rdname extract
#'
#' @param include.b,include.beta,include.ci,include.se,include.odds,include.odds.ci,include.statistic,include.p,include.stars,include.df,include.effects,conf.level,conf.method
#' Parameter fuer die Regress-Tabellen
#'
#' @export
#'
#' @examples
#' \donttest{
#' fit <- lm(Sepal.Width ~ Petal.Length + Petal.Width, iris)
#' # broom::tidy(fit)
#' # broom::glance(fit)
#' # summary(fit)
#' extract_param(fit)
#'
#' }
#'
extract_param  <- function(x,
                          include.b = TRUE,
                          include.se = TRUE,
                          include.beta = FALSE,
                          include.ci = FALSE,

                          include.odds = FALSE,
                          include.odds.ci = if(include.odds & include.ci) TRUE else  FALSE,
                          include.statistic = TRUE,
                          include.p = TRUE,
                          include.stars = FALSE,
                          include.df = FALSE,

                          include.effects = c("ran_pars", "fixed"),

                          include.eta = TRUE,
                          include.sumsq = TRUE,
                          include.meansq = FALSE,
                          include.omega = FALSE,
                          include.power =FALSE,

                          conf.int = TRUE,# if (include.ci | include.odds.ci) TRUE else  FALSE ,
                          conf.level = 0.95,
                          conf.method = "Wald",
                          fix_format = FALSE,
                          digits.param = 3,
                          digits.odds = 2,
                          digits.test = 2,
                          digits.beta = 2,
                          format="fg",
                          conf.style.1 = FALSE,
                          ...) {
#cat("\n in extract_param \n ")
  if (inherits(x, "aov"))  #  parameters::model_parameters()
    return(
      extract_param_aov(
        x,
        include.eta = include.sumsq,
        include.sumsq = include.sumsq,
        include.meansq = include.meansq,
        include.omega = include.omega,
        include.power = include.power,
        fix_format = fix_format
      )
    )


  param <-  "term"
  res <- NULL
 # Error in broom:tidy
  if (inherits(x, "lmerModLmerTest") | inherits(x, "glmerMod")) {
    res <- tidy_lmer(
      x,
      effects = include.effects,
      conf.int = conf.int,
      conf.level = conf.level,
      conf.method =  conf.method)
    res$beta <- NA
    coefs <- res
  }
  else{
     if (inherits(x, "lmerMod")) {
      res <- broom::tidy(
        x,
       # effects =  "fixed",
        conf.int = conf.int,
        conf.level = conf.level,
        conf.method =  conf.method)
      res$p.value <-NA
      res$p.value[which(res$group=="fixed")]<-
        as.vector(lmerTest:::summary.lmerModLmerTest(x)$coefficients[,5])
    } else{
      if(inherits(x, "mlm"))
      param <- c("response", "term")

      res <- broom::tidy(
      x,
      effects =  "fixed",
      conf.int = conf.int,
      conf.level = conf.level,
      conf.method =  conf.method)
      res$group <- "fixed"
      res$df <- NA
    }

    if (inherits(x, "lme")) {
      my_se <- sqrt(diag(vcov(x)))
      my_coef <- lme4::fixef(x)
      res$conf.low = my_coef - 1.96 * my_se
      res$conf.high = my_coef + 1.96 * my_se
      warning("Eigene Methode: Assuming a normal approximation for the fixed effects (1.96*standard error).")
      }
    coefs <- res
  }


  if (include.b) {
    param <- c(param,  "estimate")
    if (fix_format)
      coefs$estimate <-
        render_f(res$estimate, digits = digits.param, format = format)

   # print(    coefs$estimate )
  }

  if (include.ci) {
      if (fix_format) {
        if (conf.style.1) {
          param <- c(param,   c("conf"))
          coefs$conf <-
             rndr_CI2(
              res[, c("conf.high", "conf.low")] ,
              digits= digits.param,
              format=format)
        }
        else{
          param <- c(param, c("conf.high", "conf.low"))
          coefs$conf.high <-
            render_f(res$conf.high,
                                        digits.param, format = format)
          coefs$conf.low <-
            render_f(res$conf.low,
                                        digits.param, format = format)
        }
      } else {
        param <- c(param,   c("conf.high", "conf.low"))
      }
    }

  if (include.beta & inherits(x, "lm") & !inherits(x, "glm")) {
      param <- c(param, "beta")
      # b <- res$estimate[-1]
      #
      # sx <- sapply(x$model[-1], function(z) {
      #   if (!is.numeric(z)) {
      #     cat("\nBeta macht bei ", class(z), "keinen Sinn!\n")
      #     z <- as.numeric(z)
      #   }
      #   sd(z, na.rm = TRUE)
      # })
      # sy <- sd(x$model[[1]], na.rm = TRUE)
      # coefs$beta <- c(NA, b * sx / sy)
      # Die obere Funktion rechnet die Interaktionen falsch!

      coefs$beta <- as.vector(c(NA, coef(rockchalk::standardize(x))))

      if (fix_format)
        coefs$beta <-
        render_f(coefs$beta,
                           digits.beta,
                           format = "f")

  }

  if (include.se) {
    param <- c(param, "std.error")
    if (fix_format)
      coefs$std.error <-
        render_f(res$std.error,
                           digits.param, format = format)

  }

  if (include.statistic) {
    if (inherits(x, "glm") & conf.method == "Wald")
      coefs$statistic <- (res$estimate / res$std.error) ^ 2

    param <- c(param, "statistic")
    if (fix_format)
      coefs$statistic <- render_f(res$statistic,
                                            digits.test, format = "f")
  }

  if (include.odds & inherits(x, "glm")) {
    param <- c(param, "odds")
    if (fix_format)
      coefs$odds <-
        rndr_ods(exp(res$estimate),  digits.odds)
    else
      coefs$odds <- exp(res$estimate)

    if (coefs[1, 1] == "(Intercept)")
      coefs$odds[1] <- NA

  }

  if (include.odds.ci & inherits(x, "glm")) {
    coefs$odds.conf.low <-  res$odds.conf.low <- exp(res$conf.low)
    coefs$odds.conf.high <-
      res$odds.conf.high <- exp(res$conf.high)

    if (fix_format) {
      if (conf.style.1) {
        param <- c(param, c("odds.conf"))
        coefs$odds.conf <-
          rndr_CI2(res[, c("odds.conf.low", "odds.conf.high")] ,
                              digits = digits.odds,
                              format = "f")
        if (res[1, 1] == "(Intercept)")
          coefs$odds.conf[1] <-  NA
      }
      else{
        param <- c(param, c("odds.conf.low", "odds.conf.high"))
        coefs$odds.conf.low <-
          rndr_ods(coefs$odds.conf.low, digits.odds)

        coefs$odds.conf.high <-
          rndr_ods(coefs$odds.conf.high , digits.odds)


        if (res[1, 1] == "(Intercept)") {
          coefs$odds.conf.low[1] <-  NA
          coefs$odds.conf.high[1]  <- NA
        }
      }
    }
    else{
      param <- c(param, c("odds.conf.low", "odds.conf.high"))
      if (coefs[1, 1] == "(Intercept)") {
        coefs$odds.conf.low[1] <-  NA
        coefs$odds.conf.high[1]  <- NA
      }

    }

  }

  if (include.stars) {
    param <- c(param, "stars")
    coefs$stars <- rndr_Stars(res$p.value)
  }

  if (include.p) {
    param <- c(param, "p.value")
    if (fix_format)
      coefs$p.value <-
        rndr_P(res$p.value, symbol.leading = c("", "<"))
  }

  tibble::as_tibble(coefs[param])
}



#' @param x Objekt
#' @param effects,scales,ran_prefix,conf.int,conf.level,conf.method Param antidy
#' @param ... extra params
#'
#' @return data.frame tibble
#'
#' @noRd
tidy_lmer <- function(x,
                      effects = c("ran_pars", "fixed"),
                      scales = NULL,
                      ## c("sdcor",NA),
                      ran_prefix = NULL,
                      conf.int = FALSE,
                      conf.level = 0.95,
                      conf.method = "Wald",# should be one of “profile”, “Wald”, “boot”
                      ...) {

  rslt<-broom.mixed::tidy(x,
                          effects = effects,
                          scales = scales,
                          ran_prefix = ran_prefix,
                          conf.int = conf.int,
                          conf.level = conf.level,
                          conf.method=conf.method)

  rslt$group<- ifelse( is.na(rslt$group), rslt$effect, rslt$group)
  rslt <- rslt[c(3:ncol(rslt), 2)]

  rslt
}



#' @rdname extract
#'
#' @export
#'
#'
extract_param_aov <- function(x,
                               include.eta = FALSE,
                               include.sumsq = TRUE,
                               include.meansq = FALSE,
                               include.omega = FALSE,
                               include.power = FALSE,
                               fix_format = FALSE,
                               ...) {

  cat("\n  extract_param_aov\n")
  # include.term <-
  #   include.df <- include.f <- include.chi <- include.p <- TRUE
  if (inherits(x, "manova")) {
    include.sumsq <- include.meansq <- FALSE
    my_names <- c(
      "Parameter" = "term",
      "Pillai" = "pillai",
      "df" = "df",
      "F" = "statistic",
      # "Chi2" ="statistic",
      "Eta2_partial" = "eta.sq.part",
      "Omega2" =  "omega",
      "Power" = "power",
      "p" = "p.value"
    )[c(
      TRUE,TRUE,TRUE,TRUE,
      include.eta,
      include.omega,
      include.power,
      TRUE
    )]
  }
  else
    my_names <- c(
      "Parameter" = "term",
      "Mean_Square" = "meansq",
      "Sum_Squares" = "sumsq",
      "df" = "df",
      "F" = "statistic",
      # "Chi2" ="statistic",
      "Eta2_partial" = "eta.sq.part",
      "Omega2" =  "omega",
      "Power" = "power",
      "p" = "p.value"
    )[c(
      TRUE,
      include.meansq,include.sumsq,
      TRUE,TRUE,
      include.eta,
      include.omega,
      include.power,
      TRUE
    )]
  #rslt <- broom::tidy(x)
  rslt <-
    parameters::model_parameters(
      x,
      eta_squared = include.eta,
      power = include.power,
      omega_squared  = include.omega
     # epsilon_squared
     # type	car::Anova()
     # ci	for  omega_squared, eta_squared
    )

  rslt <- plyr::rename(
    rslt,
    replace = my_names,
    warn_missing = FALSE,
    warn_duplicated = FALSE
  )

  if (fix_format) {
    rslt <- transform(
      rslt,
      statistic   = render_f(statistic , digits = 2),
      df  = render_f(df, digits = 0),
      p.value = rndr_P(p.value,
                                    include.symbol = FALSE)
    )

    if (include.meansq)
      rslt$meansq <- render_f(rslt$meansq, digits = 2)

    if (include.sumsq)
      rslt$sumsq <- render_f(rslt$sumsq, digits = 2)

    if (include.eta)
      rslt$eta.sq.part <- render_f(rslt$eta.sq.part, digits = 2)

    if (include.omega)
      rslt$omega.part <- render_f(rslt$omega.part, digits = 2)

    if (include.power)
      rslt$power <-  render_f(rslt$power,  digits = 2)
  }


  return(tibble::as_tibble(rslt[my_names]))
}

#
# extract_param_aov <- function(x,
#                               include.eta = TRUE,
#                               include.sumsq = TRUE,
#                               include.meansq = FALSE,
#                               fix_format = FALSE,
#                               digits.test = 2,
#                               format = "f",
#                               ...) {
#   param <- "term"
#   res <- broom::tidy(x)
#
#   if (!include.sumsq) {
#     param <- c(param, "sumsq")
#     if (fix_format)
#       res$sumsq <-
#         render_f(res$sumsq, digits = 2, format = format)
#   }
#
#   if (!include.meansq) {
#     param <- c(param, "meansq")
#     if (fix_format)
#       res$meansq <-
#         render_f(res$meansq, digits = 2, format = format)
#   }
#
#   param <- c(param, c("df", "statistic"))
#   if (include.eta) {
#     if (is(x, "lm") | is(x, "anova")) {
#       param <- c(param, c("eta.sq", "eta.sq.part"))
#       k <- ncol(res)
#       myeta <-  extract_etaSqr(x, 2, FALSE)
#
#       if (nrow(myeta) != nrow(res))
#         stop(
#           "extract_param_aov() mit etaSquared eventuell liefert car:Anova(lm(...)) das richtige Ergebniss"
#         )
#       res <- cbind(res[,-k], myeta , res[k])
#
#       if (fix_format) {
#         res$eta.sq <-
#           render_f(res$eta.sq,
#                              digits = digits.test, format = format)
#         res$eta.sq.part <-
#           render_f(res$eta.sq.part,
#                              digits = digits.test,
#                              format = format)
#       }
#     }
#   }
#
#   param <- c(param, "p.value")
#
#   if (fix_format) {
#     res$statistic <-
#       render_f(res$statistic, digits = digits.test, format = format)
#
#     res$p.value <-
#       rndr_P(res$p.value, symbol.leading = c("", "<"))
#     res$df <-
#       render_f(res$df, digits = 0, format = format)
#   }
#   tibble::as_tibble(res[param])
# }





#' eta Sqr: Effsize Measures of association
#'
#' Pearson's r correlation Small 0.2,  Medium 0.5, Large 0.8
#'r2 coefficient of determination Small 0.04, Medium 0.25, Large 0.64
#'
#'  Quelle: http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3444174/pdf/i1949-8357-4-3-279.pdf
#'
#'  Gestolen von https://cran.r-project.org/web/packages/effsize/effsize.pdf
#'
#' @rdname extract
#' @export
#' @param x Objekt oder Formel
#' @param ... weitere Optionen
#'
extract_effsize<- function(x, ...){
  if(inherits(x, "formula") | is.numeric(x)) cohens_d(x, ...)
  else extract_etaSqr(x, ...)
}


#' @rdname extract
#' @export
#' @examples
#'
#' \donttest{
#' # etaSquared
#'
#' fit1<-lm(y1~x1, anscombe)
#'
#' # extract_etaSqr(aov (y1~x1, anscombe), anova=TRUE)
#' # extract_etaSqr(fit1, anova=TRUE)
#' extract_etaSqr(fit1)
#' }
extract_etaSqr <-
  function (x, type = 2, anova = FALSE, ...)
    ##   <environment: namespace:lsr
  {
    if (!is(anova, "logical") | length(anova) != 1) {
      stop("\"anova\" must be a single logical value")
    }
    if (!is(x, "lm")) {
      if (is(x, "anova")) {
        #extract_etaSqr(fit1)
        eta <- effectsize::eta_squared(x)
        eta_p <-  effectsize::eta_squared(x, partial = TRUE)

        #  if (nrow(eta) == 1) {
        res <- rbind(cbind(eta[, 2],
                           eta_p[, 2]), NA)
        rownames(res)  <- c(unlist(eta[, 1]), "Residuals")
        # } else{
        #  res <- rbind(cbind(eta[-nrow(eta), 2],
        #                      eta_p[-nrow(eta), 2]), NA)
        #  rownames(res)  <- c(unlist(eta[-1, 1]), "Residuals")
        #}
        colnames(res) <-  c("eta.sq", "eta.sq.part")


        return(res)

      } else {
        stop("\"x\" must be a linear model object")
      }
    }
    if (!is(type, "numeric") | length(type) != 1) {
      stop("type must be equal to 1,2 or 3")
    }
    if (type == 1) {
      ss <- anova(x)[, "Sum Sq", drop = FALSE]
      ss.res <- ss[dim(ss)[1],]
      ss.tot <- sum(ss)
      ss <- ss[-dim(ss)[1], , drop = FALSE]
      ss <- as.matrix(ss)
    }
    else {
      if (type == 2) {
        ss.tot <- sum((x$model[, 1] - mean(x$model[, 1])) ^ 2)
        ss.res <- sum((x$residuals) ^ 2)
        terms <- attr(x$terms, "factors")[-1, , drop = FALSE]
        l <- attr(x$terms, "term.labels")
        ss <- matrix(NA, length(l), 1)
        rownames(ss) <- l
        for (i in seq_along(ss)) {
          vars.this.term <- which(terms[, i] != 0)
          dependent.terms <- which(apply(terms[vars.this.term,
                                               , drop = FALSE], 2, prod) > 0)
          m0 <- lm(x$terms[-dependent.terms], x$model)
          if (length(dependent.terms) > 1) {
            m1 <- lm(x$terms[-setdiff(dependent.terms,
                                      i)], x$model)
            ss[i] <- anova(m0, m1)$`Sum of Sq`[2]
          }
          else {
            ss[i] <- anova(m0, x)$`Sum of Sq`[2]
          }
        }
      }
      else {
        if (type == 3) {
          mod <- drop1(x, scope = x$terms)
          ss <- mod[-1, "Sum of Sq", drop = FALSE]
          ss.res <- mod[1, "RSS"]
          ss.tot <- sum((x$model[, 1] - mean(x$model[,
                                                     1])) ^ 2)
          ss <- as.matrix(ss)
        }
        else {
          stop("type must be equal to 1, 2 or 3")
        }
      }
    }
    if (anova == FALSE) {
      ss <- rbind(ss, ss.res)
      eta2 <- ss / ss.tot
      eta2p <- ss / (ss + ss.res)
      k <- length(ss)
      E <- cbind(eta2, eta2p)
      E[k, 2] <- NA
      colnames(E) <- c("eta.sq", "eta.sq.part")
      rownames(E) <- rownames(ss)
      rownames(E)[k] <- "Residuals"

      # eta2 <- ss/ss.tot
      # eta2p <- ss/(ss + ss.res)
      # E <- cbind(eta2, eta2p)
      # rownames(E) <- rownames(ss)
      # colnames(E) <- c("eta.sq", "eta.sq.part")
    }
    else {
      ss <- rbind(ss, ss.res)
      eta2 <- ss / ss.tot
      eta2p <- ss / (ss + ss.res)
      k <- length(ss)
      # eta2p[k] <- NA
      df <- anova(x)[, "Df"]
      ms <- ss / df
      Fval <- ms / ms[k]
      p <- 1 - pf(Fval, df, rep.int(df[k], k))
      E <- cbind(ss, df, ms, Fval, eta2, eta2p, p)
      E[k, c(4, 6, 7)] <- NA
      colnames(E) <- c("SS", "df",
                       "MS", "F", "eta.sq", "eta.sq.part", "p")

      rownames(E) <- rownames(ss)
      rownames(E)[k] <- "Residuals"
    }
    return(E)
  }


#' @rdname extract
#' @noRd
#' @examples
#' \donttest{
#'  x <- rnorm(10, 10, 1)
#'  y <- rnorm(10, 5, 5)
#'
#' cohens_d(x, y)
#'
#' # varanax <-Melt2(m1+m2~nr, varana, key="time", value="m")
#' # cohens_d(m~time, varanax )
#' }
cohens_d <- function(x, ...) {
  UseMethod("cohens_d")
}

#' @rdname extract
#' @noRd
cohens_d.default <- function(x, y, ...) {
  lx <- length(x) - 1
  ly <- length(y) - 1
  md  <-
    abs(mean(x) - mean(y))        ## mean difference (numerator)
  csd <- lx * var(x) + ly * var(y)
  csd <- csd / (lx + ly)
  csd <- sqrt(csd)                     ## common sd computation

  c(cohens_d = md / csd)                        ## cohen's d
}

#' @rdname extract
#' @noRd
cohens_d.formula = function(x, data = list(), ...) {
  #mf <- model.frame(formula=x, data=data)
  mf <- aggregate(x, data, function(x) {
    x <- na.omit(x)
    c(l = length(x) - 1,
      m = mean(x),
      var = var(x))
  })[[2]]

  #return(mf[,"l"])
  md  <-
    abs(mf[1, "m"] - mf[2, "m"])        ## mean difference (numerator)
  csd <- mf[1, "l"] * mf[1, "var"] + mf[2, "l"] * mf[2, "var"]
  csd <- csd / (mf[1, "l"] + mf[2, "l"])
  csd <- sqrt(csd)                     ## common sd computation

  c(cohens_d = md / csd)                        ## cohen's d

}





