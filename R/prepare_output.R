#' prepare_output
#'
#' Ergebnis fuer Output vorbereiten
#'
#' @param x data.frame Objekt Liste oder df
#' @param caption Uberschrift
#' @param note Anmerkung
#' @param N Anzahl
#' @param labels Label
#' @param rgroup,n.rgroup an htmlTable {htmlTable}
#' @param ... Weitere Argumente
#' @return tibble mit attributen
#' @export
prepare_output <- function(x,

                           caption = "",
                           note = "",
                           N = NULL,
                           labels = NA,
                         #  include.n =  get_opt("caption"),
                           rgroup = NULL,
                           n.rgroup = NULL,
                          # class = NULL,
                           ...) {




  # <deprecated>
  #   message: The `x` argument of `as_tibble.matrix()`
 # must have unique column names if `.name_repair` is omitted as of tibble 2.0.0.
  #Using compatibility `.name_repair`.
  # Backtrace:
  #   1. base::source("~/.active-rstudio-document", echo = TRUE)
  # 6. stp25stat2:::Tbll_xtabs.formula(~case, infert) R/tbll_xtable.R:38:2
  # 7. stp25stat2:::Tbll_xtabs.xtabs(x_tab, ...) R/tbll_xtable.R:128:2
  # 8. stp25stat2::prepare_output(...) R/tbll_xtable.R:225:2
  # 10. tibble:::as_tibble.matrix(x)

  if (is.null(note))  note <- ""
  if (is.null(caption)) caption <- ""
 # if (!is.null(include.n) & !is.null(N))  caption <- paste0(caption, " (N=", N, ")")

  attr(x, "caption") <-  caption
  attr(x, "note") <- note
  attr(x, "N") <- N
  attr(x, "labels") <- labels
  attr(x, "rgroup") <- rgroup
  attr(x, "n.rgroup") <- n.rgroup
 # attr(x, "col_header") <- NULL

  x <-  tibble::as_tibble(x, .name_repair = "unique", rownames=NA)
  #if(!is.null(class)) class(x) <- c(class(x), class)
#  if(!is.null(get_lang()))
#    names(x) <- get_lang(names(x))

  x
}
