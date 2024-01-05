
#' @rdname Tbll_desc
#'
#' @description  Tbll_desc_long
#'
#' @param include.range lang oder kurz
#' @param measure.name "m"
#' @param include.custom eigene Funktion function(x){ mean(x)}
#' @export
#'
#' @examples
#'
#'
#' #mtcars |> Tbll_desc(mpg, cyl,  disp,  hp, drat,
#' #                    wt,  qsec, vs, am, gear, carb)
#' mtcars |> Tbll_desc_long(
#'   mpg[mean, 1],
#'   cyl[median],
#'   "Hallo",
#'   disp[0],
#'   hp[0],
#'   drat,
#'   wt,
#'   qsec[1],
#'   vs[freq],
#'   am[freq],
#'   gear,
#'   carb,
#'   include.range = TRUE,
#'   include.n = TRUE
#' )
#'
#' mtcars |> Tbll_desc_long(
#' mpg,
#' cyl,
#' drat,
#' wt,
#' gear,
#' carb,
#' # by=~gear,
#'
#' include.custom = function(x, by, ...){
#'   x <- as.numeric(x)
#'   c(mean= mean(x, na.rm=TRUE), sd=sd(x))
#' }
#'
#' )
#' mtcars$G <- factor(mtcars$vs, 0:1, c("A", "B"))
#'
#' mtcars <- stp25tools::Label(
#'   mtcars,
#'   mpg	= "Miles/(US) gallon",
#'   cyl	= "Number of cylinders",
#'   disp =	"Displacement (cu.in.)",
#'   hp	= "Gross horsepower",
#'   drat =	"Rear axle ratio",
#'   wt	= "Weight (1000 lbs)",
#'   qsec =	"1/4 mile time",
#'   vs	= "Engine (0 = V-shaped, 1 = straight)",
#'   am	= "Transmission (0 = automatic, 1 = manual)",
#'   gear =	"Number of forward gears",
#'   carb =	"Number of carburetors"
#' )
#'
#' mtcars |> Tbll_desc_long(
#'   mpg[mean, 1],
#'   cyl[median],
#'   "Hallo",
#'   disp[0],
#'   hp[0],
#'   drat,
#'   wt,
#'   qsec[1],
#'   vs[freq],
#'   am[freq],
#'   gear,
#'   carb,
#'   by =  ~ G,
#'   include.range = TRUE,
#'   include.n = TRUE,
#'   include.label=FALSE
#' )
#'
Tbll_desc_long <- function(...,
                           include.range = TRUE,
                           include.n = TRUE,
                          # include.test = FALSE,
                           include.label = TRUE,
                           include.custom = NULL,

                           digits = NULL,
                           abbreviate = TRUE
                         #  measure.name = "m"
                          ) {

  X <- stp25tools::prepare_data2(...)

  type <- if (include.range) "auto_long" else "auto_kurz"


  if(is.null(include.custom))
  rslt <-
    analyse_sesc_long(
      X,
      type = type,
      measure.name = get_opt("table", "measure.name.m"),
      digits = digits,
      include.label = include.label,
      abbreviate =  abbreviate
    )
  else{
    cat("\ncustom_fun\n")
    include.n <- FALSE
  #  include.test <- FALSE
    X$measure <- ifelse(X$measure == "header", "header", "custom_fun")
    rslt <-
    analyse_sesc_long(
      X,
      type = "custom_fun",
      fun = include.custom,
    #  measure.name = "custom_fun",
    #  measure.name = get_opt("table", "measure.name.m"),
    #  digits = digits,
      include.label = include.label,
      abbreviate =  abbreviate
    )}




  if (include.n) {
    X$measure <- ifelse(X$measure == "header", "header", "custom_fun")
    rslt_n <- analyse_sesc_long(X,
                                type = "custom_fun",
                                fun = length2,
                                measure.name = "n")
    rslt <-  if (ncol(rslt) == 2)
      cbind(rslt[1], rslt_n[2], rslt[-1])
    else
      cbind(rslt[1:2], rslt_n[3], rslt[3])
  }
  names(rslt)[1] <- "Item"

  prepare_output(rslt, note = "", N = X$N)
}




analyse_sesc_long <- function(X,
                       measure.name = get_opt("table", "measure.name.m"),
                       fun = NULL,
                       digits = NULL,
                       type =  "auto_kurz",
                       include.label=FALSE,
                       abbreviate = FALSE)
{
  rslt <- NULL

  for (i in seq_len(length(X$measure))) {
    if (X$measure[i] == "factor") {
      if (!is.factor(X$data[[X$measure.vars[i]]])) {
        X$data[[X$measure.vars[i]]] <- factor(X$data[[X$measure.vars[i]]])
        # warning("Konvertiere die Variable ", X$measure.vars[i], " zu Factor!")
      }

      lvl <- levels(X$data[[X$measure.vars[i]]])
      if(abbreviate) lvl <- abbreviate(lvl)

      X$row_name[i] <- paste0(X$row_name[i], " (",
                              paste0(lvl,
                                     collapse = "/"), ")")
    }
    else if (X$measure[i] %in%  c("mean", "numeric")) {
      X$row_name[i] <- paste0(X$row_name[i], get_opt("mean", "include_name"))
    }
    else if (X$measure[i] == "median") {
      X$row_name[i] <- paste0(X$row_name[i], get_opt("median", "include_name"))
    }
    else if (X$measure[i] == "logical") {
      X$data[[X$measure.vars[i]]] <-
        factor(X$data[[X$measure.vars[i]]], c(TRUE, FALSE))
      X$measure[i] <- "factor"
      X$row_name[i] <- paste0(X$row_name[i], " (",
                              paste0(levels(X$data[[X$measure.vars[i]]]),
                                     collapse = "/"), ")")
    }


    rslt[[X$measure.vars[i]]] <-
      berechne_all(
        X$data,
        X$measure.vars[i],
        X$by,
        X$measure[i],
        type,
        fun = fun,
        digits = if (is.null(digits)) X$digits[i] else digits,
        measure.name = measure.name
      )
  }

  rslt <- plyr::ldply(rslt)
  if (include.label)
    rslt[[1]] <-
    as.character(factor(rslt[[1]], names(X$row_name), X$row_name))
  rslt

}




# Helper ------------------------------------------------------------------

#' Aggregate
#'
#'
#' Interne Funktion um Mittelwerte/Freq zu berechnen.
#'
#' @noRd
#' @param x  measure.vars
#' @param type "auto_long"
#' @param fm formel
#' @param digits Nachkommastellen
#' @param by,measure,measure.name aus prepare Formula
#' @param fun Function an plyr::ddply
#'
berechne_all <- function(data,
                         x,
                         by = "1",
                         measure,
                         type = "1",
                         fun = function(x)length(na.omit(x)),
                         fm = NULL,
                         digits = get_opt("mean","digits" ),
                         measure.name = NULL
) {


  mdn <- function() {
    aggregate(
      fm,
      data,
      FUN = function(x) {

        if (type == "auto_long")
          rndr_median_iqr_range(
            median(x, na.rm = TRUE),
            ifelse(length(x) > 2, IQR(x, na.rm = TRUE), NA),
            min(x, na.rm = TRUE),
            max(x, na.rm = TRUE),
            digits = digits
          )
        else
          rndr_median(median(x, na.rm = TRUE),
                                 ifelse(length(x) > 2, IQR(x, na.rm = TRUE), NA),
                                 digits = digits)

      }
    )
  }

  mn <- function() {
    aggregate(
      fm,
      data,
      FUN = function(x) {
        if (type == "auto_long")
          rndr_mean_range(
            mean(x, na.rm = TRUE),
            ifelse(length(x) > 2, sd(x, na.rm = TRUE), NA),
            min(x, na.rm = TRUE),
            max(x, na.rm = TRUE),
            digits = digits
          )
        else
          rndr_mean(mean(x, na.rm = TRUE),
                    ifelse(length(x) > 2, sd(x, na.rm = TRUE), NA),
                    digits=digits)
      }
    )
  }

  frq <- function() {
    aggregate(
      fm,
      data ,
      FUN = function(x) {
        r <- table(x)
        paste(r, collapse = "/")
      }
    )
  }

  lgcl <- function() {
    aggregate(
      fm,
      data ,
      FUN = function(x) {
        x <- factor(x)
        r <- table(x)
        paste(r, collapse = "/")
      }
    )
  }

  custom_fun <- function() {
    res <-  aggregate(fm, data, FUN = fun, simplify = TRUE)

    if (is.matrix(res[[ncol(res)]])) {
      measure.name <<- NULL
      cbind(res[-ncol(res)],  res[[ncol(res)]])
    } else
      res
  }

  emty <- function() {
    data.frame(x = "", stringsAsFactors = FALSE)
  }

  if (is.null(fm)) {
    fm <- stp25tools:::make_formula(x, by)
  }

  res <- switch (
    measure,
    factor = frq() ,
    numeric = mn(),
    median = mdn(),
    integer = mn(),
    mean = mn(),
    units=mn(), ## library(units)
    custom_fun = custom_fun(),
    logical = lgcl(),
    header = emty(),
    emty()
  )

  if (!is.null(measure.name))
    names(res)[ncol(res)] <- measure.name[1]

  res
}




