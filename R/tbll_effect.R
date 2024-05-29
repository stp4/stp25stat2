#' Effect Displays
#'
#' Workaraund fuer effect {effects}. Dei Funktion erlaubt formulas und kann  ~a*b
#'  sowie ~b*a evaluieren.
#' @param x    fit  und effecte x = mod	(a regression model objec)  term	(the quoted name of a term)
#' @param ... param an effect transformation = list(link = log, inverse = exp)
#' @param include.se,include.ci,include.n,digits an Output
#'
#' @return data.frame ore  list with data.frame
#' @export
#'
#' @examples
#'
#' \donttest{
#' require(effects)
#'
#' mod <- lm(prestige ~ type * (education + income) + women, Prestige)
#' Tbll_effect(mod, ~ type * education + women)
#'
#' DF <- data.frame(
#'   y=c(12, 15, 23, 44, 11, 44, 49, 27, 25,  8,
#'       11, 10, 28, 15, 34, 20, 31, 55, 15, 34,
#'       47, 11, 27,  9, 12, 15,  7, 15, 42, 14,
#'       19, 24, 20, 10, 38, 28, 36,  9, 31),
#'   iq =c(91,  95, 103, 116,  88, 116, 118, 106, 105,  82,
#'         88,  87, 107,  95, 111, 100, 109, 120,  95, 111,
#'         117,  88, 106,  85,  91,  95,  81,  95, 115,  94,
#'         99, 104, 100,  87, 113, 107, 112,  84, 109 )
#' )
#' DF$log.y =  log(DF$y)
#'
#'
#' (fit.linear <- lm(y ~ iq, DF))
#' (fit.log.y <- lm(log.y ~ iq, DF))
#' (fit.model.log<- lm(log(y)~iq, DF))
#'
#' p1 <-
#'   plot(effect("iq", fit.linear ,
#'               partial.residuals = TRUE),
#'        main = "y (linear)")
#'
#' p2 <-
#'   plot(effect("iq", fit.log.y,
#'               partial.residuals = TRUE),
#'        main = "log")
#'
#' p3 <- plot(effect("iq",  fit.log.y,
#'                   partial.residuals = TRUE,
#'                   transformation =  list(link =  log,  inverse = exp)),
#'            main = "log + trans")
#'
#' p4 <- plot(effect("iq", fit.model.log ,
#'                   partial.residuals = TRUE),
#'            main = "log(y)")
#'
#' p5 <-   plot(effect("iq", fit.model.log ,
#'                     partial.residuals = TRUE,
#'                     transformation =  list(link =  log,  inverse = exp)),
#'              main = "log(y) + trans")
#'
#' require(cowplot)
#' plot_grid(p1,p4, p5,  p2, p3, ncol = 3)
#'
#' cbind(
#'   Tbll(effect("iq", fit.linear), include.ci=FALSE),
#'   log.y=Tbll(effect("iq", fit.log.y), include.ci=FALSE)[[2]],
#'   log.y.trans= Tbll(effect("iq", fit.log.y,
#'                            transformation = list(link = log, inverse = exp)),
#'                            include.ci=FALSE)[[2]],
#'   model.log=Tbll(effect("iq", fit.model.log), include.ci=FALSE)[[2]],
#'   model.log.trans= Tbll(effect("iq", fit.model.log,
#'                                transformation =  list(link = log, inverse = exp)),
#'                                include.ci=FALSE)[[2]]
#' )
#'
#' }
#'
Tbll_effect <-
  function(x,
           term = NULL,
           ...,
           include.se = FALSE,
           include.ci = TRUE,
           include.n = FALSE,
           digits = 2) {


    if (is.null(term)) {
      tbll_extract_eff(
        effects::allEffects(x, ...),
        include.se = include.se,
        include.ci = include.ci,
        include.n = include.n,
        digits = digits
      )
    }
    else {
      if (inherits(term, "formula")) {
        trm <- gsub(" ", "", strsplit(as.character(term), "\\+")[[2L]])
        term <- list()

        for (i in trm) {
          term[i]  <-    strsplit(i, "\\*")
        }
      } else if (!is.character(term)) {
        stop("Nur Formulas oder Character sind erlaubt!")
      }
      if (length(term) == 1) {
        tbll_extract_eff(
          effects::effect(term = term[[1]],  mod = x, ...),
          include.se = include.se,
          include.ci = include.ci,
          include.n = include.n,
          digits = digits
        )
      } else{
        rslt <- list()
        for (i in seq_along(term)) {
          rslt[[names(term)[i]]] <-
            tbll_extract_eff(
              effects::effect(term = term[[i]], mod = x, ...),
              include.se = include.se,
              include.ci = include.ci,
              include.n = include.n,
              digits = digits
            )
        }
        rslt
      }
    }
  }


#' @rdname extract
#'
#' @param transformation an effects::allEffect
#' @param names erste Spalte
#' @param xlevels an effects::allEffects()
#' @export
#' @description
#' Extrahiert die Effectte (magrinal-means) mit effects::allEffects
#' alternative ist  marginaleffects::marginal_means aber hier können nur
#'  Factoren untersucht werden.
#'
#' @return data.frame(term, level, fit, se,lower, upper, value)
#' Ausgabe der Effekte als dataframe zum erstellen von Grafiken
#'
#' @examples
#'
#' set.seed(1)
#'
#' g<- gl(2, 8, labels = c("Control", "Treat"))
#' x<- rnorm(2*8)
#' y<- rnorm(2*8)/10 + x
#' fit1<-lm(y~x)
#' fit2<-lm(y~x+g)
#'
#' extract_effect(fit1, xlevels=list(x=c(-3,0,1)))
#' extract_effect(fit1, fit2, xlevels=list(x=c(-1,0,1)))
#'
extract_effect <-
  function(...,
           names = NULL,
           transformation,
           xlevels=list()) {
  fit <- list(...)
  if (is.null(names)) {
    names <- abbreviate(gsub("[~??+\\:=]", "",
                             as.character(
                               as.list(
                                 sys.call()
                                 ))[seq_len(length(fit)) + 1]),
                        minlength = 7)
  }
  rslt <- NULL

  for (i in seq_along(fit)) {
    rslt[names[i]] <-
      purrr::pmap(list(mod = fit[i]),
                  extraxt_all_effects,
                  transformation = transformation,
                  xlevels = xlevels)
  }

  stp25tools::Rbind2(rslt)
}

#-
extraxt_all_effects <- function(mod, ...){
  extract_effect2(effects::allEffects(mod, ...))
  }

extract_effect2 <- function(x,
                            ...,
                            .id = "term") {
  rslt <-
    tbll_extract_eff(x, ..., return.data.frame = TRUE)
  data <- dplyr::bind_rows(rslt, .id = .id)


  if (length(data) == 8) {
    data[.id] <- names(data)[2]
    data[[2]] <- as.character( data[[2]] )
    names(data)[2] <- "level"
  }
  else {
    data <-
      cbind(data[1],
            tidyr::unite(data[names(data)[-c(1, 3:8)]], level, na.rm = TRUE),
            data[3:8])

  }
  data
}


#' @rdname extract
#' @export
#' @examples
#'
#' \donttest{
#' require(effects)
#' fit1 <-
#'   lm (Sepal.Length ~ Sepal.Width * Species + Petal.Width, data = iris)
#' fit2 <-
#'   lm (log(Sepal.Length) ~ Sepal.Width * Species + Petal.Width, data = iris)
#'
#' x1 <- allEffects(fit1)
#' x2 <- allEffects(fit2,
#'                  transformation = list(link = log, inverse = exp))
#' x3 <- effect("Petal.Width",  fit1)
#' x4 <- effect("Petal.Width",  fit2,
#'              transformation = list(link = log, inverse = exp))
#' Tbll(x1)
#' Tbll(x3)
#' Tbll(x2)
#' Tbll(x4)
#'
#' }
#'
tbll_extract.eff <- function(x,
                             ...) {
  tbll_extract_eff(x, ...)
}


#' @rdname Tbll_effect
#' @export
#'
tbll_extract.efflist <- function(x, ...) {
    tbll_extract_eff(x, ...)
  }


#' @param x,i Werte ueber die for schleife
#'
#' @noRd
extract_n <- function (x, i) {
  if (inherits(x, "eff"))
    x <- list(i = x)
  y <- names(x[[i]]$data)[1L]

  fm <-
    formula(paste0(y, "~", paste0(names(x[[i]]$variables), collapse = "+")))

  var_is_factor <-
    lapply(x[[i]]$variables, function(z)
      z$is.factor)

  var_source <- lapply(x[[i]]$variables, function(z)
    z$levels)

  for (j in names(var_source)) {
    if (!var_is_factor[[1]]) {
      breaks <- var_source[[j]]
      breaks <- rowMeans(cbind(dplyr::lag(breaks)[-1],
                               dplyr::lead(breaks)[-length(breaks)]))

      x[[i]]$data[[j]] <-
        cut(x[[i]]$data[[j]], c(-Inf, breaks, Inf))
    }
  }

  rslt_n <- aggregate(
    fm,
    x[[i]]$data,
    FUN = function(n)
      length(n),
    drop = FALSE
  )

  rslt_n[[ncol(rslt_n)]]
}

#' @param x objekt
#' @param type fuer transform "response", "link"
#'
#' orginal geht nicht  "Tue Apr 20 11:53:58 2021"
#' primär habe ich x$transformation$inverse ergaenzt
#'
#' @noRd
effects_as.data.frame.eff <-
  function (x,
            type = c("response", "link")) {

    type <- match.arg(type)
    linkinv <- if (is.null(x$link$linkinv))
      I
    else
      x$link$linkinv
    linkmu.eta <- if (is.null(x$link$mu.eta))
      function(x)
        NA
    else
      x$link$mu.eta
    xx <- x$x
    for (var in names(xx)) {
      if (is.factor(xx[[var]])) {
        xx[[var]] <- addNA(xx[[var]])
      }
    }
    x$x <- xx

    result <- switch(type,
                     response = {
                       data.frame(
                         x$x,
                         fit = x$transformation$inverse(x$fit),
                         se = x$transformation$inverse(x$fit) * x$se,
                         lower = x$transformation$inverse(x$lower),
                         upper = x$transformation$inverse(x$upper)
                       )
                     }, link = {
                       data.frame(
                         x$x,
                         fit = x$fit,
                         se = x$se,
                         lower = x$lower,
                         upper = x$upper
                       )
                     })
    attr(result, "type") <- type
    result
  }


#' @param x objekt
#' @param include.fit,include.se,include.ci,include.n include
#' @param digits Nachkomastellen
#' @param type fuer transform "response", "link"
#' @param ... nicht benutzt abfangen von zn note
#'
#'    das ist die eigendliche Funktion die sowol efflist als auch eff  aufloest.
#'
#' @noRd
#'
tbll_extract_eff <-
  function(x,
           include.fit = TRUE,
           include.se = FALSE,
           include.ci = TRUE,
           include.n = FALSE,
           digits = 2,
           type = c("response", "link"),
           return.data.frame = FALSE,
           ...)  {
    caption <- "Regression Models: Effect"
    note <- ""
    type <- match.arg(type)
    rslt <- NULL
    if (inherits(x, "eff")) {
      rslt[[1]] <- effects_as.data.frame.eff(x, type = type)
    }
    else{
      rslt <-  lapply(x, effects_as.data.frame.eff, type = type)
    }

    for (i in seq_along(rslt)) {
      if (include.fit & include.ci) {
        rslt[[i]]$value <-
          rndr_mean_CI(rslt[[i]]$fit,
                       cbind(rslt[[i]]$lower, rslt[[i]]$upper),
                       digits = digits)
        note <- "Average [95%-CI]"
      }
      else if (include.fit & include.se) {
        rslt[[i]]$value  <-
          rndr_mean(rslt[[i]]$fit,  rslt[[i]]$se, digits)
        note <- "Average (SE)"
      }
      else if (include.fit) {
        rslt[[i]]$value <-
          render_f(rslt[[i]]$fit, digits = digits)
        note <- "Average"
      }
      else {
        return(rslt[[i]])
      }

      if (include.n) {
        rslt[[i]]$value <-
          paste0("(", extract_n(x, i), ") ", rslt[[i]]$value)
      }

      if (!return.data.frame) {
        rslt[[i]] <-  rslt[[i]][-(1:4 + (ncol(rslt[[i]]) - 1 - 4))]

        if (ncol(rslt[[i]]) == 2) {
          rslt[[i]] <-
            prepare_output(rslt[[i]],
                           caption = caption,
                           note = note)
        }
        else {
          rslt[[i]] <- prepare_output(
            tidyr::pivot_wider(rslt[[i]],
                               names_from = 2,
                               values_from = "value"),
            caption = caption,
            note = note
          )
        }
      }
      else{
        rslt[[i]]$n <- extract_n(x, i)
        nn <- length(rslt[[i]])
        rslt[[i]] <- rslt[[i]][c(1:(nn - 2), nn, nn - 1)]
      }
    }

    if (length(rslt) == 1)  rslt[[1]]
    else  rslt
  }
