
#' Regression Table
#'
#' Bei glm  kann mit dem Parameter conf.method Wald oder z-test fuer
#' die Test statistik gewaelt werden.
#'
#'
#' @param ... ein oder mehrer Modelle
#' @param include.label  Namen oder Labels FALSE
#' @param names Spaltenname
#' @param digits  Nachkomastellen
#' @param include.b,include.ci  estimate
#' @param include.se SE
#' @param include.beta st. Beta
#' @param include.odds,include.odds.ci Odds
#' @param include.statistic Test Statistik
#' @param conf.method fuer glm zb "Wald"
#' @param include.p,include.stars P-Werte,Sternchen
#' @param include.param Ausgabe der Parameter
#' @param include.gof  ModellGuete ausgeben
#' @param include.r,include.pseudo R-Quadrat
#' @param include.aic,include.bic Guetemasse
#' @param include.loglik,include.minus.LL  fuer glm
#' @param include.test Anova Test fuer das gesamt Modell
#' @param include.rmse RMSE
#' @param include.custom Eigene Spalte
#' @export
#'
#' @examples
#'
#'  lm1 <- lm(breaks ~ wool + tension, data = warpbreaks)
#'  lm2 <- lm(breaks ~ wool * tension, data = warpbreaks)
#'
#'  Tbll_reg(
#'   lm1, lm2,
#'   include.p = FALSE,
#'   include.ci = TRUE,
#'   include.se=FALSE
#' )
#'
#'
Tbll_reg  <- function(...,
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
  models <- NULL
  fit <- list(...)

  if (inherits(fit[[1]], "rowwise_df")) {
    if (length(fit) != 1)
      warning(
        "\n\nEtwas ist faul: es ist eine liste der Laenge = ",
        length(fit),
        " gekommen!\n",
        "Ich erwarte hier ein nest_by Modell."
      )
    # hier kommt
    # nest_by(cyl) |>
    #  mutate(models = list(lm(mpg ~ hp, data)))
    fit <- fit[[1]]
    fit <- fit |> dplyr::pull(models, name = names(fit[1]))

    if (is.null(names)) {
      names <- abbreviate(gsub("[~??+\\:=]", "", names(fit)),
                          minlength = 7)
    }
  } else if (is.null(names)) {
    names <- abbreviate(gsub("[~??+\\:=]", "",
                             as.character(as.list(sys.call(
                             )))[seq_len(length(fit)) + 1]),
                        minlength = 7)
  }

  if(inherits(fit[[1]], "coxph" ) ){
    include.rmse <- FALSE
  }


 regression_table(
      fit,
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
#'
#' @return data.frame
#'
#' @noRd
#'
regression_table <-
  function (x,
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
    caption <- "Regression"
    note <- ""

    if (is.null(custom.model.names) |
        length(custom.model.names) != n)
      custom.model.names <- paste0("m", 1:n)

    response <-  list()
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
      response[i] <- insight::find_terms(x[[i]])$response


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
    if(!is.null(response))
      note <- paste(note,"Response: ", paste(unique(response), collapse =", "))


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


      if (n > 1)
        gofs <- stp25tools::list_to_df(gofs, last = "Obs")
      else
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

      }
      else if (!include.param) {
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

    }
    else {
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
