


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


#' @noRd
#'
#' @param x data.frame
#' @examples
#'
#'   DF2 <-   data.frame(
#'   R1 = factor(c("A", "A", "A", "C", "C", "A"),   c("A", "B", "C", "D")),
#'   R2 = factor(c("B", "B", "B", "A", "B", "D"),   c("A", "B", "C", "D")),
#'   R3 = factor(c("C", "C", "C", "B", "A", "B"),   c("A", "B", "C", "D"))
#'   )
#'   transpose3(DF2)
#'
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




#' @noRd
#' @examples
#' htest, xtable, anova
#' fix_data_frame_2(
#'   Source = c("A", "B"),
#'   Statistik = c(23.36589, 145.2254),
#'   df= c(3.36589, 5.2254) ,
#'   p = c(.036589, .002254)
#' )
#'
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



#' @rdname render_f
#'
#' @param pattern_pval,pattern_est,pattern_df,pattern_N  fix_format: pattern Sonderzeichen mit \\ schreiben!
#' @export
#'
#' @examples
#'
#' # fix_format
#'
#' df2 <- data.frame(
#' term = c("A", "B", "C", "D"),
#' Estimate = c(23.5, .14, 5.6, 2.9876),
#' df1 = c(3.3, 35., 7.8, 2.1),
#' df = c(3, 35, 7, 2),
#' N = c(33, 35, 78, 21),
#'
#' F.value = c(2.73, 12.444, 14.576, 30.412),
#' pvalue = c(0.73, 0.044, 0.056, 0.042),   stringsAsFactors =FALSE
#'
#'
#' )
#'
#' x1<-fix_format(df2)
#'
#'
fix_format <- function(x,
                        digits = NULL,
                        ptt_pval = c("Pr\\(\\>",
                                     "Pr\\.\\.",
                                     "p\\.value",
                                     "p value",
                                     "pvalue"),
                        ptt_est = c("Estimate",
                                    "Std\\. Error",
                                    "est.std",
                                    "se"),
                        ptt_df = c("Df"),
                        ptt_N = c("N",
                                  "n"),

                        ...) {
  # if (!tibble::is_tibble(x))    x <- tibble::as_tibble(x)
  is_num <- sapply(x, is.numeric)

  vars <- tolower(names(x))
  vars[which(!is_num)] <- "Character"


  if (is.null(digits)) {
    ptt_pval <- paste(tolower(ptt_pval), collapse = '|')
    ptt_est <- paste(tolower(ptt_est), collapse = '|')
    ptt_df <- paste(tolower(ptt_df), collapse = '|')
    ptt_N <- paste(tolower(ptt_N), collapse = '|')
    pval <- which(stringr::str_detect(vars, ptt_pval))
    est <- which(stringr::str_detect(vars, ptt_est))
    dgr <- which(stringr::str_detect(vars, ptt_df))
    cnt <- which(stringr::str_detect(vars, ptt_N))
    fstat <-
      setdiff(seq_along(vars)[is_num], c(est, pval, dgr, cnt))


    if (length(cnt) > 0) {
      x[cnt] <- render_f(x[cnt] , digits = 0)
    }

    if (length(pval) > 0) {
      for (i in pval) {
        x[[i]] <-
          rndr_P(x[[i]], include.symbol = FALSE)
      }
    }

    if (length(est) > 0) {
      x[est] <-
        render_f(x[est], digits = get_opt("Fstat", "digits"))
    }

    if (length(fstat) > 0) {
      x[fstat] <- render_f(x[fstat],
                           digits = get_opt("Fstat", "digits"))
    }

    if (length(dgr) > 0) {
      for (i in dgr) {
        x[[i]] <-
          render_f(x[[i]],
                   digits = ifelse(any((x[[i]] %% 1 > 0)), 1, 0))
      }
    }
  }
  else{
    if(length(digits)==1)
      x <- stp25stat2::render_f(x, digits = digits)
    else if( !is.null(names(digits))){

      for( i in names(digits)) {
        j <- which(names(x) == i)

        x[[j]] <-
          render_f(x[[j]], digits= digits[[i]])
      }
    }
    else{ warning( "Ich weis nicht wie ich die digits interpretieren soll?" ) }
  }
  x
}

