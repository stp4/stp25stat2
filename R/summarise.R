#' Summarise Data
#'
#' Summarise: ist eine  Erweiterung  aggregate()
#'
#' @export
#' @param ... An Long() daten Formeln, Variablen-Namen
#' @param fun  function(x) length(na.omit(x)) an aggregate
#' @param key,value,include.label  an Long
#' @param na.action  an aggregate
#' @param formula Zeilen/Spalten Lang/Weit
#' @param include.total,margins, logical or formula: value ~ variable + sex
#'
#' @param margins_name  character
#' @return data.frame, tibble
#' @export
#' @examples
#'
#' df <- data.frame(
#'   month = rep(1:3, 2),
#'   student = rep(c("Amy", "Bob"), each = 3),
#'   A = c(9, 7, 6, 8, 6, 9),
#'   B = c(6, 7, 8, 5, 6, 7)
#' )
#'
#' Summarise(A + B ~ student, df)
#'
#' \donttest{
#' data(hyper, package = "stp25data")
#'
#' mean3 <- function(x)
#'   round(mean(x, na.rm = TRUE), 2)
#'
#' Summarise(
#'   df,
#'   A, B,
#'   by = ~ month,
#'   fun = mean3,
#'   formula = month ~ variable,
#'   margins = TRUE
#' )
#'
#'
#' Summarise(
#'   chol0 + chol1 + chol6 + chol12 ~ g,
#'   hyper,
#'   fun = mean,
#'   key = "Zeit",
#'   value = "Cholesterin"
#' )
#'
#' Summarise(
#'   chol0 + chol1 + chol6 + chol12 ~ g,
#'   hyper,
#'   fun = mean,
#'   formula = variable ~ g
#' )
#'
#' Summarise(
#'   chol0 + chol1 + chol6 + chol12 ~ g,
#'   hyper,
#'   fun = function(x)
#'     c(m = mean(x), sd = sd(x)),
#'   magin = TRUE,
#'   formula = variable ~ g
#' )
#'
#' hyper |>
#'   Summarise(
#'     chol0, chol1, chol6, chol12,
#'     by = ~ g + med,
#'     fun = function(x)
#'       c(m = mean(x), sd = sd(x)),
#'     formula = ~ g + med
#'   )
#'
#'   }
#'
Summarise <- function(...,
                      fun = function(x)
                        length(na.omit(x)),
                      key = "variable",
                      value = "value",
                      na.action = na.pass,
                      formula = NULL,
                      include.total = FALSE,
                      margins = include.total,
                      margins_name = "Total",
                      include.label = TRUE) {

  value <- make.names(value)
  key <- make.names(key)
  values_from <- value
  molten <-
    stp25tools::Long(...,
                     key = key,
                     value = value,
                     use.label = include.label)

  default_formula <-
    formula(paste(value, "~",
                  paste(names(molten)[-ncol(molten)],
                        collapse = "+")))
# wegen na.action ansonsten
# aggregate(molten[value], molten[-ncol(molten)], FUN =fun)
  rslts <-
    aggregate(default_formula,
              molten,
              FUN = fun,
              na.action = na.action)

  rslts <- rslts[order(rslts[[1]]),]

  rst <- rslts[ncol(rslts)]

  if (class(rst[[1]])[1] == "matrix") {
    nmbr_msr <- colnames(rst[[1]])
    values_from <- dimnames(rst[[1]])[[2]]
    rslts <-
      cbind(rslts[-ncol(rslts)], rst[[1]])
  } else{
    names(rslts)[ncol(rslts)] <- value
  }

  if (isTRUE(margins) | plyr::is.formula(margins) ) {
   # cat( "\n in magrins\n")
      if(!plyr::is.formula(margins))
      if( ncol(molten) == 2)  margins <- formula(paste(value, "~1"))
      else margins <-  formula(paste(value, "~", key))

    rslts_m <-
      aggregate(margins,
                molten,
                FUN = fun,
                na.action = na.action)

    rst <- rslts_m[ncol(rslts_m)]

    if (class(rst[[1]])[1] == "matrix")
      rslts_m <- cbind(rslts_m[-ncol(rslts_m)], rst[[1]])
    # moegliche position fuer  Total
    pos_total <-  seq_len(nrow(rslts_m)) + nrow(rslts)
    rslts <- dplyr::bind_rows(rslts, rslts_m)

    # moegliche Variablen fuer  Total
    pos_variable <-  seq_len( which(names(rslts) == key))


    for (i in pos_variable) {
      if(  all(is.na(rslts[pos_total, i] ))){
      if (is.factor(rslts[[i]]))
        rslts[[i]] <-
          factor(rslts[[i]], c(margins_name, levels(rslts[[i]])))
       rslts[pos_total, i] <- margins_name

       }
    }

  }

  if (!is.null(formula)) {
    rhs <- check_formula(formula, key, value)
    rslts <- tidyr::pivot_wider(rslts,
                                names_from = !!rhs,
                                values_from = all_of(values_from))
  }

  if(any(is.na(names(rslts))))
    names(rslts)[is.na(names(rslts))] <- "NA"
  tibble::as_tibble(rslts)
}


check_formula <- function(formula, key, value) {
  if (key != "variable" | value != "value") {
    print(list(formula = formula, c(key = key, value = value)))
    stop("Sorry das geht nicht key und value duerfen nicht veraendert werden!")
  }

  if (length(formula) == 3)
    all.vars(formula[-2])
  else
    all.vars(formula)

}


