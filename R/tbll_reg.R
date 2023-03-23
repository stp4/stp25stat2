
#'  Regression Table
#'
#'  Bei glm  kann mit dem Parameter conf.method Wald oder z-test für
#'  die Test statistik gewaelt werden.
#' @export
#'
#' @examples
#'
#'  lm1 <- lm(breaks ~ wool + tension, data = warpbreaks)
#'  lm2 <- lm(breaks ~ wool * tension, data = warpbreaks)
#'
#'
#'  Tbll_reg(
#'   lm1,
#'   lm2,
#'   include.p = FALSE,
#'   include.ci = TRUE,
#'   include.se=FALSE,
#'   caption = "Regression Table"
#' )
#'
#' Tbll(lm1, lm2)
#'
#'
#' #' lm1 <- lm(breaks ~ wool + tension, data = warpbreaks2)
#'
#'
#' Tbll_reg(lm(breaks ~1, data = warpbreaks2),
#'          lm1, lm2)
#'
Tbll_reg  <- function(...,
                      caption = "",
                      include.label = FALSE,
                      # geht nicht
                      names = NULL,
                      digits = NULL,
                      include.b = TRUE,
                      include.se = FALSE,
                      include.beta = FALSE,
                      include.ci = TRUE,
                      include.odds = FALSE,
                      include.odds.ci = FALSE,
                      include.statistic=FALSE,
                      conf.method = "Wald",  # c("Wald", "Z") bei glm
                      include.p = FALSE,
                      include.stars = if (include.p) FALSE  else TRUE,

                      include.param = TRUE,
                      include.gof = TRUE,

                      include.r = TRUE,
                      include.pseudo = FALSE,
                      include.aic = TRUE,
                      include.bic = include.aic,
                      include.loglik = FALSE, include.minus.LL=include.loglik,
                      include.test = FALSE,
                      include.rmse = TRUE,

                      include.custom = NULL) {

  fit <- list(...)
  if( is.null(names)){
    names <- abbreviate(
    gsub("[~??+\\:=]", "",
         as.character(as.list(sys.call()))[seq_len(length(fit))+1]),
    minlength=7
    )
  }

  if(inherits(fit[[1]], "coxph" ) ){
    include.rmse <- FALSE
  }


 regression_table(
      fit,
      caption = caption, note = "",
      custom.model.names = names,
      include.param=include.param,
      include.gof=include.gof,
      include.custom = include.custom,
      include.b = include.b, include.se = include.se, include.beta = include.beta,
      include.ci = include.ci,
      include.odds = include.odds, include.odds.ci=include.odds.ci,
      include.statistic = include.statistic,
      include.p = include.p, include.stars = include.stars,
      include.df = FALSE,
      include.effects = c("ran_pars", "fixed"),
      ci.level = .95, conf.method = conf.method,
      digits=digits, digits.param = 3,  digits.odds = 2, digits.test = 2, digits.beta = 2,
      format = "fg",
      include.r = include.r, include.pseudo = include.pseudo,
      include.rmse = include.rmse,
      include.sigma = FALSE,
      include.variance = FALSE,
      include.devianze = FALSE,
      include.loglik = include.loglik, include.minus.LL=include.minus.LL,
      include.test = include.test,
      include.aic = include.aic, include.bic = include.bic,
      include.nobs = TRUE,
      rgroup = c("Parameter", "Goodness of fit"),
      dictionary = c(std.error = "SE", estimate = "b", p.value = "p")
    )


}




#' @rdname Tbll_reg
#' @export
Tbll_reg_long  <- function(...,
                           caption = "",
                           include.label = FALSE,
                           # geht nicht
                           #names = NULL,
                           digits = NULL,
                           include.b = TRUE,
                           include.se = FALSE,
                           include.beta = FALSE,
                           include.ci = TRUE,
                           include.odds = FALSE,
                           include.odds.ci = FALSE,
                           include.statistic = TRUE,
                           conf.method = "Wald",
                           # c("Wald", "Z") bei glm
                           include.p = FALSE,
                           include.stars = if (include.p)
                             FALSE
                           else
                             TRUE,
                           include.r = TRUE,
                           include.pseudo = FALSE,
                           include.aic = TRUE,
                           include.bic = include.aic,
                           include.param = TRUE,
                           include.gof = FALSE,
                           #  include.ftest=TRUE,
                           include.loglik = FALSE, include.minus.LL=include.loglik,
                           #include.minus.LL=include.loglik,
                           # include.r=FALSE, #include.pseudo=FALSE,
                           # include.heteroskedasticity = TRUE,
                           # include.durbin = TRUE,
                           # include.levene = FALSE,
                           # include.bartlett = FALSE,
                           # include.vif=FALSE,
                           # include.sigma=FALSE,
                           # include.rmse=FALSE,
                           # include.aic = TRUE,
                           # include.bic = TRUE,
                           #  include.residual=TRUE,
                           #  include.normality=FALSE,
                           #  include.multicollin=include.vif,
                           #  include.deviance=TRUE,
                           include.custom = NULL) {
  fit <- list(...)


  regression_table(
    fit,
    caption = caption,
    note = "",
    # custom.model.names = names,
    include.param = include.param,
    include.gof = include.gof,
    include.custom = include.custom,
    include.b = include.b,
    include.se = include.se,
    include.beta = include.beta,
    include.ci = include.ci,
    include.odds = include.odds,
    include.odds.ci = include.odds.ci,
    include.statistic = include.statistic,
    include.p = include.p,
    include.stars = include.stars,
    include.df = FALSE,
    include.effects = c("ran_pars", "fixed"),
    ci.level = .95,
    conf.method = conf.method,
    digits = digits,
    digits.param = 3,
    digits.odds = 2,
    digits.test = 2,
    digits.beta = 2,
    format = "fg",
    include.r = include.r,
    include.pseudo = include.pseudo,
    include.rmse = TRUE,
    include.sigma = FALSE,
    include.variance = FALSE,
    include.devianze = FALSE,
    include.loglik = include.loglik,  include.minus.LL=include.minus.LL,
    include.test = FALSE,
    include.aic = include.aic,
    include.bic = include.bic,
    include.nobs = TRUE,
    rgroup = c("Parameter", "Goodness of fit"),
    dictionary = c(
      std.error = "SE",
      estimate = "b",
      p.value = "p"
    )
  )


}



#' Formatierte Regressionstabelle
#'
#' Formatiert Listen mit Modellen zu Dataframs.
#'
#' @param x Regressionsobjekt
#' @param caption,note,output,custom.model.names,rgroup An Output
#' @param include.param,include.gof,include.custom Was soll ausgegeben werden
#' @param include.b,include.beta,include.ci,include.odds,include.se,include.statistic,include.odds.ci,include.p,include.stars an extract_coef
#' @param include.effects,conf.level,conf.method an extract_coef
#' @param digits.param,digits.odds,digits.test,digits.beta,formatan extract_coef
#' @param include.df,include.r,include.pseudo,include.rmse,include.sigma,include.variance,include.devianze,include.loglik,include.aic,include.bic,include.nobs,include.test An extract_goff()
#' @param ...  nicht benutzt
#'
#' @return data.frame
#'
#' @noRd
#'
regression_table <-
  function (x,
            caption = "" ,
            note = "",

            custom.model.names = NULL,
            include.param = TRUE,
            include.gof = TRUE,
            include.custom = NULL,
            include.b = TRUE,
            include.se = TRUE,
            include.beta = FALSE,
            include.ci = FALSE,
            include.odds = FALSE,
            include.odds.ci = FALSE,
            include.statistic = FALSE,
            include.p = FALSE,
            include.stars = TRUE,
            include.df = FALSE,
            include.effects = c("ran_pars", "fixed"),
            conf.level = 0.95,
            conf.method = "Wald",
            digits = NULL,
            digits.param = 3,
            digits.odds = 2,
            digits.test = 2,
            digits.beta = 2,
            format = "fg",
            include.r = TRUE,
            include.pseudo = TRUE,
            include.rmse = TRUE,
            include.sigma = FALSE,
            include.variance = FALSE,
            include.devianze = FALSE,
            include.loglik = FALSE, include.minus.LL=include.loglik,

            include.test = FALSE,
            include.aic = TRUE,
            include.bic = include.aic,
            include.nobs = TRUE,
            rgroup = c("Parameter", "Goodness of fit"),
            dictionary = c(std.error = "SE",
                           estimate = "b",
                           p.value = "p"),
            col_names = NULL,
            ...)
  {
    n <- length(x)
    coefs <- list()
    gofs <- list()
    result <- NULL
    first_nam <- ""

    if (is.null(custom.model.names) |
        length(custom.model.names) != n)
      custom.model.names <- paste0("m", 1:n)


    #-- param ----------------------------------
    for (i in seq_along(x)) {
      if (!is.null(digits)) {
        format <- "f"
        if (is.list(digits)) {
          digits.param = digits[[i]]
          #  digits.odds = digits[[i]]
        } else{
          digits.param = digits
          #  digits.odds = digits
        }
      }
      model <- extract_param(
        x[[i]],
        include.b = include.b,
        include.se = include.se,
        include.beta = include.beta,
        include.ci = include.ci,

        include.odds = include.odds,
        include.odds.ci = include.odds.ci,
        include.statistic = include.statistic,
        include.p = include.p,
        include.stars = include.stars,
        include.df = include.df,

        include.effects = include.effects,
        conf.int = TRUE ,
        conf.level = conf.level,
        conf.method = conf.method,

        digits.param = digits.param,
        digits.odds = digits.odds,
        digits.test = digits.test,
        digits.beta = digits.beta,
        format = format,
        fix_format = TRUE,
        conf.style.1 = TRUE

      )

      if (include.stars) {
        pos_star <-  grep("stars", names(model))
        model[[2]] <- paste0(unlist(model[[2]]), model[[pos_star]])
        model <- model[-pos_star]
      }

      names(model) <- sapply(names(model),
                             function(y) if (y %in% names(dictionary)) dictionary[y] else y,
                             USE.NAMES = FALSE)
      first_nam <- names(model)[2]
      coefs[[custom.model.names[i]]] <- model
    }


    if (n > 1) {
      coefs <-  stp25tools::list_to_df(coefs)
    } else
      coefs <- coefs[[1]]

    # rownames(coefs) <- NULL


    #-- gof ----------------------------------

   # cat("\n vor GOF\n")
    if (include.gof) {
      for (i in seq_along(x)) {
    #    cat("\n Schleife\n")


        model <- extract_gof(
          x[[i]],
          include.ftest = include.test,
          include.loglik = include.loglik,
          include.minus.LL = include.minus.LL,
          include.r = any(c(include.r, include.pseudo)),
          include.heteroskedasticity = FALSE,
          include.durbin = FALSE,
          include.levene = FALSE,
          include.bartlett = FALSE,
          include.sigma = include.sigma,
          include.rmse = include.rmse,
          include.aic = include.aic,
          include.bic = include.bic,
          include.residual = FALSE,
          include.normality = FALSE,
          include.multicollin = FALSE,
          include.deviance = include.devianze
        )


        names(model)  <-c("term", first_nam)
        gofs[[custom.model.names[i]]] <- model
      }


      if (n > 1) {
        gofs <- stp25tools::list_to_df(gofs, last = "Obs")
      } else
        gofs <- gofs[[1]]



      if (!is.null(include.custom)) {
        if (inherits(include.custom, "data.frame")) {
          names(include.custom) <- names(gofs)
          gofs <- dplyr::bind_rows(gofs,
                                   tibble::as_tibble(include.custom))
        }  else if (inherits(include.custom, "list")) {
          include.custom <- t(as.data.frame(include.custom))
          include.custom <-
            cbind(include.custom = rownames(include.custom), include.custom)
          colnames(include.custom) <- names(gofs)
          include.custom <- tibble::as_tibble(include.custom)

          gofs <- dplyr::bind_rows(gofs, include.custom)
        }
        else {
          warning(" Bei include.custom sind nur data.frames oder listen erlaubt.")
        }
      }

      if (include.param & include.gof) {
        n.rgroup <- nrow(coefs)
        result <-
          prepare_output(
            dplyr::bind_rows(coefs, gofs),
            caption,
            note,
            N = NULL,
            include.n = NULL,
            rgroup = rgroup,
            n.rgroup = n.rgroup
          )

      } else if (!include.param) {
        rgroup <- n.rgroup <- NULL
        result <-
          prepare_output(
            gofs,
            caption,
            note,
            N = NULL,
            include.n = NULL,
            rgroup = rgroup,
            n.rgroup = n.rgroup
          )

      }

    } else {
      rgroup <- n.rgroup <- NULL
      result <-
        prepare_output(
          coefs,
          caption,
          note,
          N = NULL,
          include.n = NULL,
          rgroup = rgroup,
          n.rgroup = n.rgroup
        )

    }


    rownames(result) <- NULL

    result
  }
