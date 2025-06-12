#' Format Log-Linear Models
#'
#' @param ... MASS-loglm Objekt
#' @param names Modellname
#' @param include.likelihood  logical. Likelihood Ratio  LL
#' @param include.pearson  logical. Pearson X-Sqr
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' require(MASS)
#' Tbll_loglm(HairEyeColor, include.pearson = TRUE)
#'
#'
#' #require(stp25stat2)
#' Plank <- data.frame(
#'   therapy = c("H+A", "H+A+P", "H+A", "H+A+P", "H+A", "H+A+P", "H+A", "H+A+P"),
#'   mace = c("yes", "yes", "no", "no", "yes", "yes", "no", "no"),
#'   infarction = c("NSTEMI", "NSTEMI", "NSTEMI", "NSTEMI", "STEMI", "STEMI", "STEMI", "STEMI"),
#'   freq = c(7, 1, 140, 54, 63, 14, 221, 126)
#' )
#' Plank$mace_bin <- ifelse(Plank$mace == "yes", 1, 0)
#'
#' #Log-Linear Models
#' tbl <- xtabs(freq ~ therapy + mace + infarction, data = Plank)
#' # [A][B][C]
#' mod1 <- loglm(~ therapy + mace + infarction, data = tbl)
#' # [AB][C]
#' mod2 <- loglm(~ therapy + mace + infarction + therapy:mace, data = tbl)
#' # [AC][BC]
#' mod3 <- loglm(~ therapy + mace + infarction + therapy:mace + infarction:mace, data = tbl)
#' # [AB][AC][BC]
#' mod4 <- loglm(~ therapy + mace + infarction + therapy:mace +
#'                 therapy:infarction + infarction:mace, data = tbl)
#' # [ABC] Saturated
#' mod5 <- loglm(~ therapy * mace * infarction, data = tbl)
#'
#'
#' anova(mod1, mod2, mod3, mod4) |> Tbll_loglm()
#' # Die ANOVA Prüft, ob ein Model signifikant besser ist als das Null-Model (M1).
#' # M3 ist signifikant (p < 0.001) -> M3 erklärt mehr Varianz als M1
#' #
#' # Je kleiner die Deviance umso besser ist das Modell - M3 ist hier recht gut.
#' Tbll_loglm(mod3)
#' # Hier ist ein nicht-signifikanter p-Wert besser da die Hypothese einen Modell
#' # testet
#'
#'
#' glm_logistic <- glm(
#'   mace_bin ~ therapy + infarction,
#'   weights = freq,
#'   family = binomial(),
#'   data = Plank
#' )
#'
#'
#' ## Poisson-Regression (loglinear)
#' glm_poisson <- glm(
#'   freq ~ therapy * mace + infarction * mace,
#'   family = poisson(),
#'   data = Plank
#' )
#'
#' # Odds Ratio (OR
#' OR <- round(exp(coef(glm_logistic))[2:3], 2)
#' # Risk Ratio (RR)
#' RR <- round(exp(coef(glm_poisson))[5:6], 2)
#' cbind(OR, RR)
#' # MACE-no-Rate war um 57% geringer unter H+A+P2Y12 im Vergleich zu H+A.
#' # (Die Prozent werden aus dem RR berechnet => 1 - 0.43 = .57)
#'
Tbll_loglm <-
  function(...,
           names = NULL,
           include.likelihood = TRUE ,
           include.pearson = FALSE) {
    rslt <- NULL
    fits <- list(...)

    if( inherits(fits[[1]], "anova.loglm")){
      return(tbll_extract.anova.loglm(...))
    }

    caption <- "Log-Linear Models"
    if (is.null(names))
      names <- paste0("M", seq_along(fits))
    for (i in seq_along(fits)) {
      if(inherits(fits[[i]], "table")){

        fm <- formula(paste("~" , paste( names(dimnames(fits[[i]])), collapse = "+" )  ))
        fits[[i]] <-  MASS::loglm(fm, data = fits[[i]])
      }

      if (include.likelihood & !include.pearson) {
        rsl <- stp25stat2::tbll_extract(fits[[i]])[1, ]
        rsl[1] <- names[i]
        rslt <- rbind(rslt, rsl)
        caption <- "Log-Linear Models (Likelihood Ratio G2)"

      }
      else if (include.likelihood & include.pearson)
      {
        rsl <- stp25stat2::tbll_extract(fits[[i]])
        rsl <- cbind(Source = c(names[i], "" ), rsl)
        rslt <-  rbind(rslt, rsl)
      }
      else   {
        rsl <- stp25stat2::tbll_extract(fits[[i]])[2, ]
        rsl[1] <- names[i]
        rslt <-  rbind(rslt, rsl )
        caption <- "Log-Linear Models (Pearson)"
      }
    }
    stp25stat2::prepare_output(rslt, caption = caption)
  }

#' @rdname extract
#' @export
#'
tbll_extract.anova.loglm <-
  function (x, ...)
  {
    rjustify <- function(str) {
      m <- max(n <- nchar(str, "c"))
      blanks <- format(c("", str[n == m][1L]))[1L]
      paste(substring(blanks, 0L, m - n), str, sep = "")
    }
    y <- x
    y[, 5L] <- round(y[, 5L], 5L)
    R <- array("", dim(x), dimnames(x))
    for (j in 1L:5L) {
      colj <- rjustify(c(colnames(x)[j], format(y[, j])))
      R[, j] <- colj[-1L]
      colnames(R)[j] <- colj[1L]
    }
    R[1L, 3L:5L] <- ""

    forms <- attr(x, "formulae")


    note <- ""
    for (i in seq_along(forms))
      note <- paste(note, paste("Model ", i, ":", sep = ""),
                    deparse(forms[[i]], width.cutoff = 500L),
                    "\n")

    R <-
      stp25tools::fix_to_tibble(R,
                                include.rownames = TRUE)

    R[[2]] <- stp25stat2::render_f( as.numeric(R[[2]]), 1)
    R[[3]] <- as.character(as.numeric(R[[3]]))
    R[[4]] <- stp25stat2::render_f( as.numeric(R[[4]]), 1)
    R[[5]] <- as.character(as.numeric(R[[5]]))
    R[1,5] <- ""
    R[[6]] <- stp25stat2:::rndr_P( as.numeric(R[[6]]), include.symbol = FALSE )


    names(R) <- c("Source", "Deviance", "df" ,"Delta(Dev)", "Delta(df)", "P-value")
    prepare_output(R,
                   caption ="LR tests for hierarchical log-linear models",
                   note=note )

  }




