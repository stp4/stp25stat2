


#' @rdname Tbll_desc
#'
#' @param ... an prepare_data2
#' @param include.total  Gesamtanzahl
#' @param include.custom fun = function(x) Auswertung (x)
#'
#' @export
#' @examples
#'
#' # from Hmisc
#' summary(breaks ~ tension + wool, warpbreaks)
#' Tbll_mean(breaks ~ tension + wool, warpbreaks)
#'
Tbll_mean <- function(...,
                      include.total = TRUE,
                      include.custom = function(x) {
                        round(c(
                          n = length(na.omit(x)),
                          mean =  mean(x, na.rm = TRUE),
                          sd =  sd(x, na.rm = TRUE)
                        ), 2)
                      })
{
  X <-
    stp25tools::prepare_data2(...)

  if (length(X$measure.vars) != 1)
    stop("Mehere measure.vars sind noch nicht implementiert")
  rslt <- NULL
  for (i in X$group.vars) {
    fm <- formula(paste(X$measure.vars[1], " ~ ", i))
    if (!is.factor(X$data[[i]])) {
      if (is.numeric(X$data[[i]]))
        X$data[[i]] <- cut(X$data[[i]], 5)
      else
        X$data[[i]] <- factor(X$data[[i]])
    }
    #  print(fm)
    rst <-  Summarise(fm, X$data, fun = include.custom)
    rst$variable <- ""
    rst$variable[1] <-  X$col_name[i]
    names(rst)[1:2] <- c("levels", "Item")
    rslt <- rbind(rslt, rst)
  }




  if (include.total) {
    rst  <- (Summarise(formula(paste(
      "~", X$measure.vars[1]
    )), X$data, fun = include.custom))

    rslt <- rbind(rslt,
                  cbind(levels = "", Item = "Overall",  rst[-1]))
  }

  prepare_output(rslt[c(2:1, 3:ncol(rslt))],
                             caption = X$row_name[1])


}

