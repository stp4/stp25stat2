#'  Mean
#'
#' Lagemasse Berechnen
#'
#' @name Mittelwert
#' @param ...  platzhalter
#' @param na.rm,exclude fehlende werte
#' @param max_factor_length,is_true_false Argumente an calc_percent
#' @export
#' @examples
#'
#' mean2(rnorm(100))
#' sd2(rnorm(100))
#'
mean2 <-
  function(x, na.rm = TRUE) {
    mean(make_numeric(x), na.rm = na.rm)
  }


#' @rdname Mittelwert
#' @export
sd2 <-  function(x, na.rm = TRUE) {
  sd(make_numeric(x), na.rm = na.rm)
}

#' @rdname Mittelwert
#' @export
length2 <- function(x, na.rm = FALSE) {
  if (na.rm) sum(!is.na(x))
  else length(x)
}

make_numeric <- function(x) {
  if (is.numeric(x))
    return(x)
  else if (is.factor(x)) {
    if (nlevels(x) == 2)
      ifelse(x == levels(x)[1], 1, 0)
    else
      as.numeric(x)
  }
  else {
    warning("Die Werte sind eventuel falsch class=", class(x), "!")
    stp25tools::as_numeric(x)
  }
}


#' @rdname Mittelwert
#' @export
median2 <-
  function(x, na.rm=TRUE, ... ) { median(make_numeric(x), na.rm = na.rm) }




# calc_ --------------------------------------------------------------------
#' @rdname Mittelwert
#' @export
#' @examples
#' percent2(gl(2, 8, labels = c("Control", "Treat")))
percent2 <- function(x, ...) calc_percent(x, ...)

#' @rdname Mittelwert
#'
#' @description Format Prozent
#'  style = 1    50% (27)
#'  style = 2    27 (50%)
#'  style = 3    50%
#'  style = 4    27
#'  style = 5    27/54
calc_percent <- function(x,
                         digits = get_opt("prozent", "digits") ,
                         n = length(x),
                         exclude =   c(NA, NaN),
                         max_factor_length = 25,
                         style = get_opt("prozent", "style") ,
                         is_true_false = FALSE,
                         ...) {


  if(missing(x)) return( switch(style,
                                "1" = "percent (count)",
                                "2" = "count (percent)",
                                "3" = "percent",
                                "4" = "count",
                                "percent (count)" ))

  if (is.null(style)) style <- 1


  # Table Creation
  if (is.factor(x)) {
    tbl <- table(x, exclude = exclude)

   if (length(tbl) > max_factor_length) {
      naLev <- levels(x)[-(1:max_factor_length)]
     # stp25output2::Text("NA = ", paste(naLev, collapse = ", "))
      x <-
        factor(x, levels(x)[1:max_factor_length], exclude = NULL)
      x <-
        addNA(x)  #- addNA modifies a factor by turning NA into an extra level
      tbl <- table(x)
    }
  }
  else if (is.logical(x)) {
    x <- factor(x, c(TRUE, FALSE), c("true", "false"))
    is_true_false <- TRUE
    tbl <- table(x, exclude = exclude)
  }
  else {
    xt <- factor(x)
    if (nlevels(xt) > max_factor_length)
      stop("class = ", class(xt), " nlevels = ", nlevels(xt))
    else
      tbl <- table(xt, exclude = exclude)
  }

  if (n == 0) {
    if (style != "4")   {
      rslt <- rep(NA, nlevels(x))
      names(rslt) <- levels(x)
    }
    else {
      # Ich verstehe die Logik nicht mehr
      rslt <- matrix(NA, ncol = 2, nrow = nlevels(x))
      colnames(rslt) <- levels(x)
    }
  }

  rslt <-
      rndr_percent(
        x = as.vector(prop.table(tbl)) * 100,
        n = as.vector(tbl),
        digits = digits,
        style = style
      )

  names(rslt) <- names(tbl)
  if (is.null(exclude))
      names(rslt)[is.na(names(rslt))] <- "n.a."

   if (!is_true_false)  rslt else rslt[1]
}


#' @rdname Mittelwert
#'
#' @param x vector
#' @param digits nachkomastellen (2)
#' @param n laenge des vectors
#' @param style lang oder Kurz
#' @param unit Einheiten
#'
calc_mean <-  function(x,
                       digits = get_opt("mean", "digits") ,
                       n = length(x),
                       style = get_opt("mean", "style"),
                       unit=NULL) {

  if(missing(x)) return( switch(style,
                                "1" = "mean (sd)",
                                "2" = "mean (sd, range)",
                                "mean (sd)"
  ))

  if(all(is.na(x))) return(NaN)

  # handle units
  if (!is.numeric(x)) {
    x <- make_numeric(x)
  }  else if (inherits(x, "units")) {
    gr <- c("[", "]")
    unit <- paste0(gr[1], as.character(attr(x, "units")), gr[2])
    x <- units::drop_units(x)
  }

  # calc and format
  if (is.null(style)) {
    rndr_mean(mean(x, na.rm = TRUE),
              ifelse(n > 2, sd(x, na.rm = TRUE), NA),
              digits=digits,
              unit=unit)
  }
  else if (style == "1") {
    rndr_mean(mean(x, na.rm = TRUE),
              ifelse(n > 2, sd (x, na.rm = TRUE), NA),
              digits=digits,
              unit=unit)
  }
  else if (style == "2" | style == "long") {
    rndr_mean_range(
      mean(x, na.rm = TRUE),
      ifelse(n > 2, sd (x, na.rm = TRUE), NA),
      min(x, na.rm = TRUE),
      max(x, na.rm = TRUE),
      digits = digits,
      unit=unit
    )
  }
  else {
    rndr_mean(mean(x),
              ifelse(n > 2, sd(x), NA),
              digits=digits,
              unit=unit)
  }
}



#' @noRd
calc_median <-
  function(x,
           digits = get_opt("median", "digits"),
           n = length(x),
           style = get_opt("median", "style"),
           unit = NULL) {

    if(missing(x)) return( switch(style,
                                  "1" = "median (quantile)",
                                  "2" = "median (IQR, range)",
                                  "3" = "median (range)",
                                  "4" =  "median (IQR)",
                                  "median (IQR)"
    ))


    median_quantil <- function()
      rndr_median_quant(quantile(x, na.rm = TRUE),
                        digits = digits,
                        unit = unit)


    median_iqr <- function()
      rndr_median_iqr(median(x),
                      ifelse(n > 2, IQR(x), NA),
                      digits = digits,
                      unit = unit)


    median_range <-  function()
      rndr_median_range(quantile(x, na.rm = TRUE),
                        digits = digits,
                        unit = unit)

    median_iqr_range <- function()
      rndr_median_iqr_range(
        median(x, na.rm = TRUE),
        IQR(x, na.rm = TRUE),
        min(x, na.rm = TRUE),
        max(x, na.rm = TRUE),
        digits = digits,
        unit = unit
      )

    if (all(is.na(x)))
      return(NaN)

    if (!is.numeric(x)) {
      x <- make_numeric(x)
    }
    else if (inherits(x, "units")) {
      gr <- c("[", "]")
      unit <- paste0(gr[1], as.character(attr(x, "units")), gr[2])
      x <- units::drop_units(x)
    }


    if (is.null(style))
      median_quantil()
    else if (style == "IQR" | style=="4" | style == "IRQ")
      median_iqr()
    else if (style == 1)
      median_quantil()
    else if (style == 2 | style == "long")
      median_iqr_range()
    else if (style == 3)
      median_range()
    else
      rndr_median_quant()

  }



#' @noRd
#
# Confidence Intervals for Binomial Proportions
#
# The Wilson interval, which is the default, was introduced by Wilson (1927)
# and is the inversion of the CLT approximation to the family of equal tail tests of p = p0. The Wilson interval is
# recommended by Agresti and Coull (1998) as well as by Brown et al (2001).
#
ci_binom <- function(x,
                     tbl = table(x),
                     conf.level = .95,
                     sides = "two.sided",
                     method = "wilson") {
  rslt <- DescTools::BinomCI(
    tbl,
    n = sum(tbl),
    conf.level = conf.level,
    sides = sides,
    method = method
  )

  r <- rndr_CI(c( rslt[, 2] * 100, rslt[, 3] * 100))

  ifelse(rslt[, 1] <= 0, ".", r)
}



#' @noRd
#
# Confidence Intervals for Multinomial Proportions
#
# Sison, C.P and Glaz, J. (1995) Simultaneous confidence intervals
# and sample size determination for multinomial proportions.
# Journal of the American Statistical Association, 90:366-369.
#
ci_factor <- function(x,
                      tbl = table(x),
                      conf.level = .95,
                      sides = "two.sided",
                      method = "sisonglaz") {
  rslt <-   DescTools::MultinomCI(tbl,
                                  conf.level = conf.level,
                                  sides = sides,
                                  method = method)

  r <- rndr_mean_CI(rslt[, 2] * 100, rslt[, 3] * 100)

  ifelse(rslt[, 1] <= 0, ".", r)
}

