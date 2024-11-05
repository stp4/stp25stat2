#' Extract (tbll_extract)
#'
#' Turn an object into a tidy table
#'
#' @name extract
#' @param x  Objekte
#' @param digits Nachkomastellen
#' @param caption character. nicht ueberall implementiert
#' @param ...  weitere Argumente
#'
#' @param Positive_Class,type,include.I2,include.Q,include.tau,include.H,include.sumsq,include.meansq,include.omega,include.power,conf.int,fix_format,digits.param,digits.odds,digits.test,digits.beta,format,conf.style.1,anova description
#' @return data.frame
#' @export
#'
tbll_extract <- function(...) {
  UseMethod("tbll_extract")
}

#' @rdname extract
#' @export
#'
tbll_extract.default <- function(x, ...) {
  cat("\n Es ist keine Methode feur die Class",
      class(x)[1],
      " in tbll_extract() implementiert.\nIch versuche daher mal das mit broom zu extrahieren.\n")
  prepare_output(fix_format(broom::tidy(x)))

}

#' @rdname extract
#' @export
#'
tbll_extract.anova <- function(x, include.eta = TRUE, ...) {

  rslt <- x
  rslt[-ncol(rslt)] <-
    render_f(rslt[-ncol(rslt)], digits = 2)
  rslt[[ncol(rslt)]] <-
    rndr_P(rslt[[ncol(rslt)]], FALSE)
  names(rslt)[ncol(rslt)] <- "P Value"

  if (include.eta & names(x)[1] == "Sum Sq") {
    eta_sqr <- render_f(extract_effsize(x), digits = 2)
    if (row.names(x)[1] == "(Intercept)")
      eta_sqr <-
        rbind(data.frame(eta.sq = NA, eta.sq.part = NA), eta_sqr)
    rslt <- cbind(rslt[-ncol(rslt)],
                  eta_sqr,
                  rslt[ncol(rslt)])
  }

  prepare_output(stp25tools::fix_to_df(rslt, include.rownames = TRUE),
                 caption =   attr(rslt, "heading")[1])
}


#' @rdname extract
#' @export
#'
tbll_extract.data.frame <- function(x, ...) {
  prepare_output(stp25tools::fix_to_df(x, include.colnames = TRUE),
                 caption =  "")
}


#' @rdname extract
#' @export
tbll_extract.lm <- function(...) {
  Tbll_reg_long(...)
}

#' @rdname extract
#' @export
tbll_extract.aov <- function(...) {
  prepare_output(
    extract_param_aov(..., fix_format=TRUE),
    caption="Analysis of Variance"
  )
}

#' @rdname extract
#' @export
tbll_extract.manova <- function(...) {
  extract.manova(...)
}

#' @rdname extract
#' @export
#' @examples
#'
#' #  TukeyHSD
#' fm1 <- aov(breaks ~ wool + tension, data = warpbreaks)
#' TukeyHSD(fm1, "tension", ordered = TRUE) |> tbll_extract()
#' #plot(TukeyHSD(fm1, "tension"))
tbll_extract.TukeyHSD <- function(x,
                                  digits = 2,
                                  ...) {

 # tbll_extract.TukeyHSD: no visible binding for global variable
  estimate <- conf.low <- conf.high <- null.value <- adj.p.value <- NULL
  rslt <- broom::tidy(x)
  rslt <- transform(
    rslt,
    estimate    = render_f(estimate, digits = digits),
    conf.low    = render_f(conf.low, digits = digits),
    conf.high   = render_f(conf.high, digits = digits),
    null.value  = render_f(null.value, digits = 0),
    adj.p.value = rndr_P(adj.p.value, include.symbol = FALSE)
  )
  prepare_output(rslt,
                 caption = "Tukey Honest Significant Differences")
}


#' @rdname extract
#' @export
tbll_extract.aovlist <- function(x, ...) {
  if (!is.null(attr(x, "weights")))
    stop("Note: Das habe ich noch nicht implementiert\n")
  nx <- names(x)
  nx <- nx[-1]
  nx <- nx[-length(nx)]
  lx <- length(nx)
  rslt <-  extract_param_aov(x, ..., fix_format=TRUE)
  lr <- nrow(rslt)
  rslt[lr, 1] <- paste(rslt[lr, 1], "(Within)")
  rslt[1:lx, 1] <- paste("Residuals", nx)


  prepare_output(rslt[c((lx + 1):lr, 1:lx),],
                 caption = "Analysis of Variance (multiple error strata)")
}



#' @rdname extract
#' @export
#'
tbll_extract.summary.aov <- function(x,
                                     ...) {
  rslt <- fix_format(broom::tidy(x[[1]]))
  prepare_output(rslt,
                 caption = "ANOVA",
                 note = paste("contrasts: ",
                              paste(options()$contrasts,
                                    collapse = ", ")))
}




#' @rdname extract
#' @export
#'
#' @examples
#'
#' \donttest{
#' summary(lm1 <- lm(Fertility ~ ., data = swiss))
#' slm1 <- stats::step(lm1)
#' tbll_extract(slm1)
#'  require(lmerTest)
#'  m <- lmerTest::lmer(
#'   Informed.liking ~ Product * Information * Gender +
#'   (1 | Consumer) + (1 | Product:Consumer),
#'   data = ham
#'  )
#' # elimination of non-significant effects
#'  s <- lmerTest::step(m)
#'
#'  tbll_extract(s)
#'
#' }
tbll_extract.step <- function(x,
                              include.se = FALSE,
                              include.df = FALSE,
                              ...) {
  caption <- "Choose a model by AIC"

  res <- NULL
  if (any(names(x) %in% "lsmeans.table")) {
    res <- x$diffs.lsmeans.table

    if (nrow(res) == 0)
      return("Finde keine Loesung step!")
    res <- cbind(Source = rownames(res), res)

    names(res)[8] <- "p.value"
    names(res)[3] <- "SE"
    names(res)[6:7] <- c("lwr",   "upr")
    res <- res[c(1, 2, 3, 6, 7, 4, 5, 8)]
    if (!include.se)
      res <- res[,-which(names(res) == "SE")]
    if (!include.df)
      res <- res[,-which(names(res) == "DF")]
    res <-   prepare_output(fix_format(res),
                            caption = caption)

  }
  else{
    warnings("Nicht getestete Methode")
    res <- broom::tidy(x)
    res <-   prepare_output(fix_format(res),
                            caption = caption)
  }

  res
}


#' @rdname extract
#' @export
tbll_extract.table<- function(...) {Tbll_xtabs(...)}

#' @rdname extract
#' @export
#' @examples
#'
#' a <- letters[1:3]
#' tbll_extract(summary(table(a, sample(a))))
#'
tbll_extract.summary.table <- function(x,
                                       ...) {
  prepare_output( data.frame(
    Chisq = render_f(x$statistic, 2),
    df =    render_f(x$parameter,0),
    p =     rndr_P(x$p.value, FALSE)
  ),
  caption = "Test for independence of all factors (Pearson)")
}



#' @rdname extract
#' @export
tbll_extract.table<- function(...) Tbll_xtabs(...)


#' @rdname extract
#' @export
tbll_extract.htest <- function(x,
                               ...) {
  # t.test
  if (any(names(x) == "statistic")) {
    if (names(x$statistic) == "t")
      rslt <- prepare_output(
        fix_data_frame_2(
          Source = x$data.name,
          T = x$statistic,
          df = x$parameter,
          p.value = x$p.value
        ),
        caption = x$method
      )
    else
      rslt <- prepare_output(
        fix_data_frame_2(
          Source = x$data.name,
          W = x$statistic,
          p.value = x$p.value
        ),
        caption = x$method
      )
  }
  else if( x$method == "Fisher's Exact Test for Count Data"){

    rslt <- prepare_output(
      data.frame(
        Source = as.character(x$data.name),
        OR  = render_f(x$estimate),
     CI  = rndr_CI(as.vector(x$conf.int)),
        p   = rndr_P(x$p.value),
        stringsAsFactors = FALSE
      ), caption = x$method)

  }
  else{
    rslt <- x$method
  }

  rslt
}



#' @rdname extract
#' @export
#'
#' @examples
#'
#' \donttest{
#' attach(airquality)
#' Month <- factor(Month, labels = month.abb[5:9])
#'
#' ## These give warnings because of ties :
#' tbll_extract(pairwise.wilcox.test(Ozone, Month))
#'
#'}
#'
tbll_extract.pairwise.htest <- function(x,
                                        caption = paste(x$data.name, x$method),
                                        ...) {
    rslt <- stp25tools::fix_to_df(x$p.value)
    for (i in seq_along(rslt))
      rslt[[i]] <- rndr_P(rslt[[i]], FALSE)
    prepare_output(rslt,
                   caption = caption)
  }





#' @rdname extract
#' @export
#'
tbll_extract.likert <- function(...) stp25likert::Tbll_likert.likert(...)



#' @rdname extract
#' @export
#'
tbll_extract.matchit <- function (x,
                                  ...) {
  note <-
    paste0("method = ", x[["info"]]$method, ", ratio = ", x[["info"]]$ratio)
  prepare_output(
    stp25tools::fix_to_df(x$nn[c(2, 4, 5, 6),]),
    caption = "matchit",
    note = note
  )
}

#' @rdname extract
#' @param digits in extract.matchit
#' @export
#'
tbll_extract.summary.matchit <- function (x,
                                          digits = 3,
                                          ...)  {
  prepare_output(stp25tools::fix_to_df(round(x$sum.matched[, c(3, 4)], digits)),
                 caption = "Summary of balance for matched data",
                 note = "")

}



#' @rdname extract
#' @export
#'
tbll_extract.principal <- function (x,
                                    ...) {
  Tbll_pca(x, ...)
}



#' @rdname extract
#' @export
tbll_extract.confusionMatrix <-
  function(x,
           digits = 2,
           Positive_Class= x$positiv,
           ...) {

      prepare_output(
        data.frame(
          Statistic =
            c(
              "Accuracy",
              "95% CI",
              "No Information Rate",
              "P-Value [Acc > NIR]",
              "Kappa",
              "Mcnemar's Test P-Value",
              "Sensitivity",
              "Specificity",
              "Pos Pred Value" ,
              "Neg Pred Value",
              "Precision",
              "Recall",
              "F1",
              "Prevalence",
              "Detection Rate",
              "Detection Prevalence" ,
              "Balanced Accuracy",
              "Positive Class"
            ),
          estimate =
            c(
              render_f(x$overall["Accuracy"], digits),
              rndr_CI(x$overall[c("AccuracyLower", "AccuracyUpper")]),
              render_f(x$overall["AccuracyNull"], digits),
              rndr_P(x$overall["AccuracyPValue"]),
              render_f(x$overall["Kappa"], digits),
              rndr_P(x$overall["McnemarPValue"]),
              render_f(x$byClass, digits),
              Positive_Class
          ),
          stringsAsFactors = FALSE
        ),
        caption="Associated Statistics"
      )


  }



#' @rdname extract
#' @export
#'
#' @examples
#'
#' \donttest{
#' # Sensitivität: richtig positive Rate eines Tests
#' # Spezifität: richtig-negative Rate eines Tests
#'
#' tab<-matrix(c(94,40,39,40), ncol=2, byrow = TRUE)
#' tbll_extract( epiR::epi.tests(tab) )
#'
#' }
#'
tbll_extract.epi.tests <-
  function (x,
            type =
              c(
                #  ap = "Apparent prevalence",
                #  tp = "True prevalence",
                se = "Sensitivity",
                sp = "Specificity",
                pv.pos = "Positive predictive value",
                pv.neg = "Negative predictive value",
                #   lr.pos = "Positive likelihood ratio",
                #   lr.neg = "Negative likelihood ratio",
                p.tpdn = "False T+ proportion for true D-",
                p.tndp = "False T- proportion for true D+",
                p.dntp = "False T+ proportion for T+",
                p.dptn = "False T- proportion for T-",
                diag.ac = "Correctly classified proportion"

                #  p.rout ="proportion of subjects with the outcome ruled out",
                #  p.rin ="proportion of subjects with the outcome ruled in",
                #   youden = "Youden's index",
                #   nndx ="number needed to diagnose",
                #   diag.or="Diagnostic odds ratio"
              ),
            digits = 2,
            ...) {
    x$detail$statistic <-
      factor(x$detail$statistic, names(type), type)
    x$detail$est <-
      render_f(x$detail$est, digits = digits)
    x$detail$CI <-
      rndr_CI(cbind(x$detail$lower, x$detail$upper), digits = digits)


    prepare_output(
      na.omit(x$detail[c(1, 2, 5)]),
      caption =  paste0("Point estimates and ",
                        x$conf.level * 100, "%"
                        , " CIs:"),
      note = x$method
    )
  }



# tbll_extract.roc not tested

#' @rdname extract
#' @export
tbll_extract.roc <- function(x,
                             digits = 2,
                             type = c(
                               threshold = "Threshold value",
                               # tn = "True negative count",
                               #  tp = "True positive count",
                               #  fn = "False negative count",
                               #  fp = "False positive count",
                               specificity = "Specificity",
                               sensitivity = "Sensitivity",
                               accuracy = "Accuracy",
                               #    npv = "Negative Predictive Value",
                               #    ppv = "Positive Predictive Value",
                               #    precision = "Precision",
                               #   recall = "Recall",
                               tpr = "True Positive Rate",
                               fpr = "False Positive Rate",
                               tnr = "True Negative Rate",
                               fnr = "False Negative Rate",
                               fdr = "False Discovery Rate"
                               #  youden = "Youden Index"#,
                               #  closest.topleft = "Distance to the top left corner of the ROC space"
                             ),
                             ...
                             ) {
  c_crd <-  pROC::coords(x, "best", ret = names(type))
  auc <- data.frame(Source  = "AUC",  Estimate = x$auc[1])

  rslt <- t(c_crd)
  rslt <- stp25tools::fix_to_df(rslt)
  rslt[[1]] <- factor(rslt[[1]] , names(type), type)


  names(rslt) <-  names(auc)
  rslt <-    rbind(auc, rslt)
  rslt[[2]] <- render_f(rslt[[2]] , digits)

  prepare_output(rslt,
                 caption =  "ROC curve")
}

#' @rdname extract
#' @export
#'
#' @examples
#'
#' \donttest{
#' require(vcd)
#' data("Arthritis")
#' tab <- xtabs(~Improved + Treatment, data = Arthritis)
#' tbll_extract(assocstats(tab))
#' }
tbll_extract.assocstats <- function(x,
                                    ...) {

 # cat("\n in tbll_extract.assocstats\n")
  if (length(x) == 2) {
   # cat("\n length = 2 \n")
    x2 <- cor2 <- NULL
    for (i in names(x)) {
      if (is.null(x2)) {
        x2 = extract_assocstats_chisq(x[[i]])
        cor2 = extract_assocstats_corr(x[[i]])
        names(cor2)[-1] <-  i
        names(x2)[-1] <- paste0(i, "_", names(x2)[-1])
      } else{
        x22 = extract_assocstats_chisq(x[[i]])
        cor22 = extract_assocstats_corr(x[[i]])
        names(cor22)[-1] <-  i
        names(x22)[-1] <- paste0(i, "_", names(x22)[-1])
        x2 <- dplyr::bind_cols(x2, x22[-1])
        cor2 <- dplyr::bind_cols(cor2, cor22[-1])
      }
    }
    list(x2 = x2, cor = cor2)
  }
  else{
    list(x2 = extract_assocstats_chisq(x),
         cor = extract_assocstats_corr(x))
  }
}

extract_assocstats_corr <- function(x,
                                    ...) {
  prepare_output(data.frame(
    Test = c("Phi-Coefficient",
             "Contingency Coefficient",
             "Cramer's V"),
    r = render_f(
      c(x$phi,
        x$contingency,
        x$cramer),
      3
    )),
    caption = "Measures of Association Correlation Test")

}

extract_assocstats_chisq   <- function(x,
                                       ...) {

 # print(x$chisq_tests)
  prepare_output(
    data.frame(
      Test = rownames(x$chisq_tests),
      Chisq = render_f(as.vector(x$chisq_tests[, 1]), 2),
      df = render_f(as.vector(x$chisq_tests[, 2]), 0),
      p.value = rndr_P(as.vector(x$chisq_tests[, 3]), FALSE)
    ),
    caption = "Pearson Chi-Squared"
  )
}

#' @rdname extract
#' @export
#'
tbll_extract.list <- function(x,
                              ...) {
  if (inherits(x[[1]], "assocstats")) {
    tbll_extract.assocstats(x)
  }
  else{
    cat("\n Keine Methode fuer list()  vorhanden.\n\n")
    sapply(x, class)
  }
}

#' @rdname extract
#' @param include.ll.ratio,include.pearson logical. in extract.loglm
#' @export
#' @examples
#'
#' \donttest{
#' require(MASS)
#' minn38a <- xtabs(f ~ ., minn38)
#' fm <- loglm(~ 1 + 2 + 3 + 4, minn38a)  # numerals as names.
#' # deviance(fm)
#' tbll_extract(fm)
#'
#' }
tbll_extract.loglm <-  function( x,
                                 include.ll.ratio = TRUE,
                                 include.pearson = TRUE,
                                 ...) {
  ts.array <-
    rbind(c(x$lrt, x$df,
            if (x$df > 0L) 1 - pchisq(x$lrt, x$df)
            else 1),
          c(x$pearson, x$df,
            if (x$df > 0L) 1 - pchisq(x$pearson, x$df)
            else 1))

  rslt <-  data.frame(
    Test = c("Likelihood Ratio", "Pearson"),
    Chisq  = render_f(ts.array[, 1], 2),
    df = render_f(ts.array[, 2], 0),
    p.value = rndr_P(ts.array[, 3], FALSE)
  )

  if (include.ll.ratio & include.pearson)
    prepare_output(rslt, caption = "Log-Linear Model")
  else if (include.ll.ratio)
    prepare_output(rslt[1,], caption = "Log-Linear Model")
  else
    prepare_output(rslt[2,], caption = "Log-Linear Model")

}


