#' Table for Survival Objects
#'
#' Log- Rank-Test ist das Standardverfahren für einfache Gruppenvergleiche
#'
#' Cox regression: Hazard das unmittelbare Risiko
#'
#'   HR = 1: No effect
#'
#'   HR < 1: Reduction in the hazard (good prognostic factor)
#'
#'   HR > 1: Increase in Hazard (bad prognostic factor)
#'
#' @param ... survfit, survdiff, coxph
#'
#' @return data.frame or list with data.frames
#' @export
#'
#' @examples
#'
#' require(magrittr)
#' # require(stp25stat2)
#' require(survival)
#'
#' fit0 <- survfit(Surv(futime, fustat) ~ 1, data = ovarian)
#' fit1 <- survfit(Surv(futime, fustat) ~ rx, data = ovarian)
#' fit2 <-
#'   survfit(Surv(futime, fustat) ~ rx + resid.ds, data = ovarian)
#' #summary(fit1)
#' Tbll(fit0)
#' Tbll(fit1)
#' Tbll(fit2)
#'
#' Tbll(fit1, include.survival = TRUE)
#'
#' "Survival Curve Differences"
#' survdiff(Surv(futime, fustat) ~ rx, data = ovarian) %>% Tbll()
#'
#' coxph(Surv(futime, fustat) ~ rx, data = ovarian) %>% Tbll()
#'
#' Tbll_surv(
#'   fit1,
#'   fit2,
#'   data = ovarian,
#'   include.param = TRUE,
#'   include.test = TRUE
#' )
#'
Tbll_surv <- function(x, ...) {
  UseMethod("Tbll_surv")
}

#' @rdname Tbll_surv
#' @export
Tbll_surv.survfit <- function(...,
                      data = NULL,
                      names = NULL,
                      include.param = FALSE,
                      include.test = TRUE) {
  surv_fits <- list(...)
  if (is.null(names)) {
    names <- abbreviate(gsub("[~??+\\:=]", "",
                             as.character(as.list(sys.call(

                             )))[seq_len(length(surv_fits)) + 1]),
                        minlength = 7)
  }
  i <- 0
  res_test <- NULL
  res_param <- NULL

  for (m in surv_fits) {
    i <- 1 + i
    if (inherits(m, "survfit")) {
      if (is.null(data))
        stop("In Tbll_surv(..., data) muessen noch die Daten nit übergeben werden!")
      m <- coxph(formula(m$call), data)
    }

    if (include.test) {
      test_coxph <- extract_coxph_test(m)
      if (length(surv_fits) > 1)
        names(test_coxph)[-1] <-
          paste0(names[i], "_", names(test_coxph)[-1])

      if (is.null(res_test))
        res_test <- test_coxph
      else
        res_test <- cbind(res_test, test_coxph[-1])
    }

    if (include.param) {
      prm_coxph <- extract_coxph_param(m)

      if (length(surv_fits) > 1)
        names(prm_coxph)[-1] <-
          paste0(names[i], "_", names(prm_coxph)[-1])

      if (is.null(res_param))
        res_param <- prm_coxph
      else
        res_param <- cbind(res_param, prm_coxph[-1])
    }

  }

  if (include.test & include.param)
    list(res_test, res_param)
  else if (include.test)
    res_test
  else
    res_param
}


#' @rdname Tbll_surv
#' @export
Tbll_surv.coxph <- function(...,
                            names = NULL,
                            include.param = FALSE,
                            include.test = TRUE) {
  surv_fits <- list(...)
  if (is.null(names)) {
    names <- abbreviate(gsub("[~??+\\:=]", "",
                             as.character(as.list(sys.call(

                             )))[seq_len(length(surv_fits)) + 1]),
                        minlength = 7)
  }
  i <- 0
  res_test <- NULL
  res_param <- NULL

  for (m in surv_fits) {
    i <- 1 + i
    if (include.test) {
      test_coxph <- extract_coxph_test(m)
      if (length(surv_fits) > 1)
        names(test_coxph)[-1] <-
          paste0(names[i], "_", names(test_coxph)[-1])

      if (is.null(res_test))
        res_test <- test_coxph
      else
        res_test <- cbind(res_test, test_coxph[-1])
    }

    if (include.param) {
      prm_coxph <- extract_coxph_param(m)

      if (length(surv_fits) > 1)
        names(prm_coxph)[-1] <-
          paste0(names[i], "_", names(prm_coxph)[-1])

      if (is.null(res_param))
        res_param <- prm_coxph
      else
        res_param <- cbind(res_param, prm_coxph[-1])
    }

  }

  if (include.test & include.param)
    list(res_test, res_param)
  else if (include.test)
    res_test
  else
    res_param
}

#' @rdname Tbll_surv
#' @param x a formula object, with the response on the left of a ~ operator, and the terms on the right. The response must be a survival object as returned by the Surv function.
#' @param data a data.frame in which to interpret the variables named in the formula, or in the subset and the weights argument.
#' @export
#'
#' @examples
#'
#' Tbll_surv2(Surv(futime, fustat) ~ rx, data = ovarian)
#'
Tbll_surv.formula <-
  function(formula,
           data,
           include.ci = TRUE,
           digits = 2,
           include.test = TRUE,
           ...) {
    cox <- coxph(formula, data)

    list(
      survdiff =  tbll_extract.survdiff(survdiff(formula, data)),
      survfit =   tbll_extract.survfit(
        survfit(formula, data),
        include.survival = FALSE,
        include.se = FALSE,
        include.ci = include.ci,
        digits = digits
      ),
      coxph = tbll_extract.coxph(cox,
                                 include.param = FALSE,
                                 include.test = include.test),
      param = extract_coxph_param(
        cox,
        include.se = FALSE,
        include.ci = include.ci,
        include.test = include.test,
        digits = digits
      )
    )
  }



#' @rdname Tbll_surv
#' @description  Univariate Cox regression
#'
#'  stolen from www.sthda.com/english/wiki/cox-proportional-hazards-model
#'
#'  regression coefficients = the hazard (risk)
#'
#'  Hazard ratios = effect size of covariates exp(coef)
#'
#'
#' @param ... an prepare_data2
#' @param note  an Output
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' #'
#' Tbll_coxph_uni(
#'   lung,
#'   age,
#'   by = ~ time + status,
#'   include.z.test = TRUE,
#'   include.wald.test = FALSE
#' )
#' coxph(Surv(time, status) ~ age, data = lung) %>%  summary() %>% Tbll()
#'
#' Tbll_coxph_uni(lung, age, sex,
#'                ph.karno, ph.ecog, wt.loss,
#'                by = ~ time + status)
#'
#' coxph(Surv(time, status) ~ age + sex + ph.karno + ph.ecog + wt.loss,
#'       data = lung) %>%  summary() %>% Tbll()
#'
#'
#' # The variables sex, age and ph.ecog have highly statistically significant
#' # coefficients, while the coefficient for ph.karno is not significant.
#' #
#' # age and ph.ecog have positive beta coefficients, while sex has a negative
#' # coefficient. Thus, older age and higher ph.ecog are associated with poorer
#' # survival, whereas being female (sex=2) is associated with better survival.
#'
#'
Tbll_coxph_uni <-
  function(x, ...,
           include.se = FALSE,
           include.ci = TRUE,
           include.z.test = TRUE,
           include.wald.test = FALSE,
           digits = 2) {
    if (inherits(x, "formula")) {
      vrs <- all.vars(x)
      covariates <-   vrs[-c(1:2)]
      Surv_vars <-
        paste0('Surv(', vrs[1], ", ", vrs[2], ')~')
      X <- list(data = list(...)[[1]])
    }
    else{
      X <-
        stp25tools::prepare_data2(x, ...) # c("age", "sex",  "ph.karno", "ph.ecog", "wt.loss")
      covariates <-   X$measure.vars
      Surv_vars <-
        paste0('Surv(', paste0(X$group.vars, collapse = ', '), ')~')
    }

    univ_formulas <- sapply(covariates,
                            function(x)
                              as.formula(paste0(Surv_vars, x)))

    univ_models <-
      lapply(univ_formulas, function(x) {
        coxph(x, data = X$data)
      })

    univ_results <- lapply(univ_models,
                           function(x) {
                             x<- summary(x)
                             rslt <- tbll_extract.summary.coxph(
                               x,
                               include.test =  include.z.test,
                               include.se = include.se,
                               include.ci = include.ci,
                               digits = digits
                             )

                             if (include.wald.test)
                               dplyr::bind_cols(
                                 rslt,
                                 wald.test =
                                   render_f(x$wald["test"], digits = 2),
                                 p.value =
                                   rndr_P(x$wald["pvalue"], FALSE)
                               )
                             else rslt
                           })

    univ_results<- do.call(rbind.data.frame, univ_results)

    prepare_output(
      univ_results,
      caption = "Univariate Cox regression",
      note = "Regression beta coefficients, effect sizes (given as hazard ratios).
           Each factor is assessed through separate univariate Cox regressions."
    )
  }


#' @param include.survival Tabelle mit allen Ueberlebenszeiten
#' @param ...  an summary.surfit times, censored = FALSE, scale = 1, extend=FALSE
#'
#' @rdname Tbll_surv
tbll_extract.survfit <- function(x,
                                 include.survival=FALSE,
                                 include.se=FALSE,
                                 include.ci=TRUE,
                                 digits = 2,
                                 ...) {
  rslt <- extract_survfit(x, digits=digits, ...)

  rslt$median <- prepare_output(rslt$median,  caption = "Median", note="")

  if( !include.se ) rslt$table <-
    rslt$table[-(ncol(rslt$table)-2)]
   if( !include.ci )
  rslt$table <-
    rslt$table[-c((ncol(rslt$table)-1), ncol(rslt$table) )]

  if(include.survival) rslt
  else  rslt$median
}


#' @param include.se Standardfehler
#' @param include.ci  Konfidenzintervall
#'
#' @rdname Tbll_surv
tbll_extract.summary.survfit <- function(x,
                                         digits = NULL,
                                         include.se=FALSE,
                                         include.ci=TRUE) {

  include <- c(time = "time", n.risk = "n.risk", n.event = "n.event", surv = "survival")
  if(include.se) include<- append(include, c(std.err = "std.err"))
  if(include.ci) include<- append(include, c(lower = "lower 95% CI", upper = "upper 95% CI"))
  rslt <- extract_summary_survfit(x, digits=digits, percent=FALSE, include=include)
  prepare_output(rslt,  caption="Summary of a Survival Curve", note="")
}


#' @rdname Tbll_surv
tbll_extract.survdiff <- function(x) {
  prepare_output( extract_survdiff(x),
                  caption = "Test Survival Curve Differences",
                  note=APA(x)
  )
}


#' @param include.param Regrssionstabelle
#' @param include.test  z-Test
#'
#' @rdname Tbll_surv
tbll_extract.coxph <- function(x,
                               include.param = FALSE,
                               include.test = TRUE) {
  rslt <- NULL
  prm <- NULL
  if (include.test) {
    rslt <- extract_coxph_test(x)
  }
  if (include.param) {
    prm <- extract_coxph_param(x)
    if(include.test) rslt<- list(test = rslt, param = prm)
    else rslt <- prm
  }
  rslt
}

#' @rdname Tbll_surv
tbll_extract.summary.coxph <-
  function(x,
           include.se = FALSE,
           include.ci = TRUE,
           include.test = TRUE,
           digits = 2)
  {
    rslt <- data.frame(
      Source = row.names(x$coefficients),
      beta = render_f(x$coefficients[, 1], digits = digits)
    )
    if (include.se)
      rslt <-
        cbind(rslt, se = render_f(x$coefficients[, 3], digits = digits))

    rslt <-
      cbind(rslt, HR = render_f(x$coefficients[, 2], digits = 2))

    if (include.ci)
      rslt <-
      cbind(rslt, CI = rndr_CI(x$conf.int[, c("lower .95", "upper .95")]))
    if (include.test)
      rslt <- cbind(
        rslt,
        z.test = render_f(x$coefficients[, 4], digits = digits),
        p.value = rndr_P(x$coefficients[, 5], FALSE)
      )

    prepare_output(rslt,
                   caption = "Estimated Hazard Ratio Cox regression model")
  }


#' summary surv
#'
#' extract_survfit
#'
#' @param x surv
#' @param ...  an summary.surfit times, censored = FALSE, scale = 1, extend=FALSE
#'
#'
#' @noRd
#'
extract_survfit <- function(x,
                            digits = 2,
                            percent = FALSE,
                            include = c(
                              time = "time",
                              n.risk = "n.risk",
                              n.event = "n.event",
                              surv = "survival",
                              std.err = "std.err",
                              lower = "lower 95% CI",
                              upper = "upper 95% CI"
                            ),
                            ...) {

  xs<- summary(x, ...)
  mdn <-xs$table
  if (is.null(names(mdn))) {
    mdn <- as.matrix(mdn)
    mdn  <-   data.frame(
      Source =  rownames(mdn), #   sapply(strsplit(rownames(mdn), "="), "[", 2),
      median = render_f(mdn[, "median"], digits),
      low.ci = render_f(mdn[, "0.95LCL"], digits),
      up.ci = render_f(mdn[, "0.95UCL"], digits),
      Mean = rndr_mean(mdn[, "rmean"], mdn[, "se(rmean)"], digits = digits)
    )

    tab <- extract_summary_survfit(xs, digits, percent, include)

  }
  else
  {
    mdn  <-   data.frame(
      Source = "Null",
      median = render_f(mdn[["median"]], digits),
      low.ci = render_f(mdn[["0.95LCL"]], digits),
      up.ci = render_f(mdn[["0.95UCL"]], digits),
      Mean = rndr_mean(mdn[["rmean"]], mdn[["se(rmean)"]], digits = digits)
    )

    tab <-  extract_summary_survfit(xs, digits, percent, include)

  }
  list(median = mdn, table = tab)
}


extract_summary_survfit <- function(x,
                                    digits = NULL,
                                    percent = FALSE,
                                    include = c(
                                      time = "time",
                                      n.risk = "n.risk",
                                      n.event = "n.event",
                                      surv = "survival",
                                      std.err = "std.err",
                                      lower = "lower 95% CI",
                                      upper = "upper 95% CI"
                                    )) {
  if (is.null(names(include))) {
    vars <- vars_names <- include
  }
  else{
    vars <-  names(include)
    vars_names <- as.character(include)
  }
  rslt <- as.data.frame(x[vars])

  if (percent) {
    rslt$surv  <- rslt$surv * 100
    rslt$lower <- rslt$lower * 100
    rslt$upper <- rslt$upper * 100
  }

  colnames(rslt) <- vars_names
  rslt <-
    fix_format(rslt, exclude = 1:3, digits = digits)
  if ("strata" %in% names(x))
    rslt <- cbind(Source = x$strata, rslt)

  rslt
}


extract_survdiff <- function(x) {
  data.frame(
    Source = c(names(x$n), "Overall"),
    N = as.integer(c(x$n, sum(x$n))),
    Observed = as.integer(c(x$obs, sum(x$obs))),
    Expected = c(round(x$exp, 1), NA)
  )
}


extract_coxph_test <- function(x) {
  sfit <- summary(x)
  Concordance <- paste0(
    render_f(sfit$concordance[1], 2),
    " (SE=",
    render_f(sfit$concordance[2], 2),
    ")"
  )
  tst <-
    cbind(data.frame(
      Source = c(
        "Log rank test",
        "Wald test",
        "Likelihood ratio test",

        "Rsquare",
        "AIC",
        "BIC"
      )
    ),
    rbind(
      sfit$sctest,
      sfit$waldtest,
      sfit$logtest,

      c(sfit$rsq[1], NA, NA),
      c(AIC(x),  NA, NA),
      c(BIC(x), NA, NA)
    ))

tst[[2]] <- render_f(tst[[2]], 2)
tst[[4]] <- rndr_P(tst[[4]], FALSE)

 tst<- rbind( tst, c("Concordance", Concordance, NA, NA))

  prepare_output(tst, caption = "Log rank test")
}


extract_coxph_param <-
  function (x,
            include.se = FALSE,
            include.ci = TRUE,
            include.test = TRUE,
            digits = 2) {
    tbll_extract.summary.coxph(
      summary(x),
      include.se = include.se,
      include.ci = include.ci,
      include.test = include.test,
      digits = digits
    )

  }
