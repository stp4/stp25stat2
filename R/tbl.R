#' Turn an object into a table
#'
#' @param x object
#' @param digits  decimal places
#' @param caption,note an Output
#' @param ... alles an Tbll
#'
#' @return data.frame oder liste mit data.frames
#' @export
#'
#' @examples
#'
#' lm1 <- lm(breaks ~ wool + tension, data = warpbreaks)
#' fm1 <- aov(breaks ~ wool + tension, data = warpbreaks)
#' tk1 <- TukeyHSD(fm1, "tension", ordered = TRUE)
#'
#'
#' Tbll(breaks ~ wool + tension, data = warpbreaks )
#'
#' Tbll(warpbreaks,
#'      breaks,
#'      wool,
#'      tension)
#' Tbll(warpbreaks,
#'      breaks ~ wool + tension)
#'
#' Tbll(
#'   lm1,
#'   include.p = FALSE,
#'   include.ci = TRUE
#' )
#'
Tbll <- function(...)  {
  dots <- lazyeval::lazy_dots(...)
  n <- length(dots)
  rslt <- NULL

  isa_formula <- FALSE
  frst <-
    lazyeval::lazy_eval(lazyeval::make_call(quote(class), dots[[1]]))

  if (n > 1 & "data.frame" %in%  frst )
    isa_formula <-
    any(grepl("~", as.character(dots[[2]][1])))

  if (frst[1] == "formula" |
      ("data.frame" %in% frst & !isa_formula & frst[1] != "anova")) {
    rslt <- Tbll_desc(...)
  }
  else if ("data.frame" %in% frst & isa_formula) {
    if (n <= 2)
      rslt <-
        lazyeval::lazy_eval(lazyeval::make_call(
          quote(Tbll_desc), dots[c(2, 1)]))
    else
      rslt <-
        lazyeval::lazy_eval(lazyeval::make_call(
          quote(Tbll_desc), dots[c(2, 1, (3:n))]))
  }
  else{
    if (n > 1) {
      is_arg <- sapply(dots,
                       function(x) {
                         lazyeval::lazy_eval(lazyeval::make_call(
                           quote(is.vector), x))
                       })

      if (all(is_arg[-1])) {
        rslt <- tbll_extract(...)
      }
      else{
        is_obj <- which(!is_arg)
        is_arg <- which(is_arg)
        names_obj <- sapply(dots[is_obj],
                            function(x) {
                              as.character(x[1])
                            })
        for (i in seq_along(is_obj)) {
          rslt[[names_obj[i]]] <-
            lazyeval::lazy_eval(lazyeval::make_call(
              quote(tbll_extract), dots[c(i, is_arg)]))
        }
      }
    }
    else
      rslt <- tbll_extract(...)
  }

  rslt
}

