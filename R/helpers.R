# @importFrom stp25tools dapply1
#stp25tools:::dapply1

# dapply1 <-
#   function (.data,
#             fun = function(x)
#               as.numeric(x),
#             stringsAsFactors = FALSE,
#             ...) {
#     if (tibble::is_tibble(.data))
#       tibble::as_tibble(plyr::llply(.data, fun, ...))
#     else
#       data.frame(plyr::llply(.data, fun, ...),
#                  stringsAsFactors=stringsAsFactors)
#   }


#'
#'
#' @param x data.frame
#' @examples
#' \donttest{
#'   DF2 <-   data.frame(
#'   R1 = factor(c("A", "A", "A", "C", "C", "A"),   c("A", "B", "C", "D")),
#'   R2 = factor(c("B", "B", "B", "A", "B", "D"),   c("A", "B", "C", "D")),
#'   R3 = factor(c("C", "C", "C", "B", "A", "B"),   c("A", "B", "C", "D"))
#'   )
#'   transpose3(DF2)
#'   }
#'
#' @noRd
transpose3 <- function(x) {
  lvl = levels(x[[1]])
  transposed <- t(apply(x, 1, function(z) {
    trans <- NULL
    for (i in lvl) {
      tr <- which(z == i)
      if (length(tr) == 0)
        tr <- 0
      names(tr) <- i
      trans <- c(trans, tr)
    }
    trans
  }))
  # kontrolle cbind(x, transposed)
  as.data.frame(transposed)
}




#'
#' @examples
#'
#' \donttest{
#'
#' htest, xtable, anova
#' fix_data_frame_2(
#'   Source = c("A", "B"),
#'   Statistik = c(23.36589, 145.2254),
#'   df= c(3.36589, 5.2254) ,
#'   p = c(.036589, .002254)
#' )
#' }
#'
#' @noRd
fix_data_frame_2 <- function(...,
                             digits = 2) {
  rslt <- data.frame(...)
  p_value <-  ncol(rslt)
  statistik <- 2:(p_value - 1)
  rslt[[1]] <- as.character(rslt[[1]])

  # print(statistik)
  for (i in statistik)
    rslt[[i]] <-
    render_f(rslt[[i]], digits = digits)

  rslt[[p_value]] <-
    rndr_P(rslt[[p_value]], FALSE)
  rslt
}


