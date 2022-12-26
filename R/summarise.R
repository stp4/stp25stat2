#' Summarise Data
#'
#' Summarise: ist eine  Erweiterung  aggregate()
#'
#' @export
#' @param ... An Long() daten Formeln,Variablen-Namen
#' @param fun  function(x) length(na.omit(x)) an aggregate
#' @param key,value,use.label  an Long
#' @param na.action  an aggregate
#' @param formula Zeilen/Spalten Lang/Weit
#' @param margins,margins_name  Gesamt
#'
#' @return reshape objek
#' @export
#' @examples
#' #'
#' mean3 <- function(x)
#'   round(mean(x, na.rm = TRUE), 2)
#' df <- data.frame(
#'   month = rep(1:3, 2),
#'   student = rep(c("Amy", "Bob"), each = 3),
#'   A = c(9, 7, 6, 8, 6, 9),
#'   B = c(6, 7, 8, 5, 6, 7)
#' )
#'
#' Summarise(A + B ~ student, df)
#'
#' Summarise(
#'   df,
#'   A,  B,
#'   by =  ~  month,
#'   fun = mean3,
#'   formula = month ~ variable,
#'   margins = TRUE
#' )
#'
#' require(stp25data)
#' Summarise(
#'   chol0 + chol1 + chol6 + chol12 ~ g,
#'   hyper,
#'   fun = mean,
#'   key = "Zeit",
#'   value = "Cholesterin"
#' )
Summarise <- function(...,
                      fun = function(x)
                        length(na.omit(x)),
                      key = "variable",
                      value = "value",
                      na.action = na.pass,
                      formula = NULL,
                      margins = FALSE,
                      margins_name = "Total",
                      include.label = TRUE) {
  nmbr_msr <- 1
  molten <-
    stp25tools::Long(...,
                     key = key,
                     value = value,
                     use.label = include.label)

  default_formula <-
    formula(paste(value, "~", paste(names(molten)[-ncol(molten)], collapse = "+")))

  rslts <-
    aggregate(default_formula,
              molten,
              FUN = fun,
              na.action = na.action)

  rslts <- rslts[order(rslts[[1]]), ]

  rst <- rslts[ncol(rslts)]
  if (class(rst[[1]])[1] == "matrix") {
    nmbr_msr <- colnames(rst[[1]])
    rslts <-
      cbind(rslts[-ncol(rslts)], rst[[1]])
  } else{
    names(rslts)[ncol(rslts)] <- value
  }

  if (isTRUE(margins)) {
    default_formula <-
      formula(paste(value, "~",
                    paste(names(molten)[-((1:0) - ncol(molten))],
                          collapse = "+")))
    rslts_m <-
      aggregate(default_formula,
                molten,
                FUN = fun,
                na.action = na.action)

    rst <- rslts_m[ncol(rslts_m)]
    if (class(rst[[1]])[1] == "matrix")
      rslts_m <-
      cbind(rslts_m[-ncol(rslts_m)], rst[[1]])

    rslts <- dplyr::bind_rows(rslts, rslts_m)

    frst <-  rslts[[1]]
    if (is.factor(frst))
      rslts[[1]] <-
      factor(frst, c(levels(frst), margins_name))


    rslts[[1]][is.na(frst)] <- margins_name
  }

  if (!is.null(formula)) {
    rslts <- stp25tools::Wide(formula, rslts, value)
  }
  rslts
}
