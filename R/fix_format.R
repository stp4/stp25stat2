
#' @rdname render_f
#'
#' @param auto.format,pattern_pval,pattern_est,pattern_df,pattern_N  fix_format: pattern Sonderzeichen mit \\ schreiben!
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
#'  require(car)
#' mod1 <- lm(conformity ~ fcategory * partner.status, data = Moore)
#' mod2 <-
#'   glm(conformity ~ fcategory * partner.status,
#'       data = Moore,
#'       family = poisson())
#'
#' Anova(mod1) |> fix_format()
#' summary(mod1)$coefficients |> fix_format()
#' Anova(mod2) |> fix_format()
#' summary(mod2)$coefficients |> fix_format()
#'
fix_format <- function(x,
                       digits = NULL,
                       names_repair = TRUE,
                       include.rownames = TRUE,
                       p.value = c(
                         "Pr\\(\\>",
                         "p value",
                         "p.value",
                         "pvalue"),
                       se = c("Std\\. Error", "est.std"),
                       df = c("N", "Df")
) {

  x <-
    stp25tools::fix_to_tibble(x, include.rownames = include.rownames)
  data_names <- names(x)
  detect_pvalue <-
    stringr::str_starts(data_names, paste0(p.value, collapse = "|"))
  detect_se <-
    stringr::str_starts(data_names, paste0(se, collapse = "|"))
  detect_df <-   data_names %in%  df

  if (names_repair) {
    names(x)[which(detect_pvalue)] <- "p value"
    names(x)[which(detect_se)] <- "SE"
  }

  if (is.null(digits)) {
    digits <- ifelse(sapply(x, is.numeric), "numeric", NA)
    # digits[which(detect_se)] <- "numeric"
    digits[which(detect_pvalue)] <- "pvalue"
    digits[which(detect_df)] <- "integer"
  }

  purrr::map2_dfc(x, digits,
                  function(x, digits) {
                    if (is.na(digits))
                      x
                    else if (is.numeric(digits))
                      render_f(x, digits)
                    else if (digits == "pvalue")
                      rndr_P(x, include.symbol = FALSE)
                    else if (digits == "numeric")
                      render_f(x, 2)
                    else
                      render_f(x, 0)
                  })
}












# fix_format_old <- function(x,
#                        digits = NULL,
#                        auto.format = TRUE,
#                        #p.value = NULL,
#                        include.rownames= TRUE,
#                        ptt_pval = c("Pr\\(\\>",
#                                     "Pr\\(\\>F\\)",
#                                     "Pr\\.\\.",
#                                     "p\\.value",
#                                     "p value",
#                                     "pvalue"),
#                        ptt_est = c("Estimate",
#                                    "Std\\. Error",
#                                    "est.std",
#                                    "se"),
#                        ptt_df = c("Df"),
#                        ptt_N = c("N", "n"),
#
#                        ...) {
#   #x_in <- x
#   x <-  stp25tools::fix_to_tibble(x, include.rownames	=include.rownames	)
#
#
#   is_num <- sapply(x, is.numeric)
#   vars <- names(x)
#   vars[which(!is_num)] <- "Character"
#   #print(is_num)
#
#   if (!is.null(digits)) {
#
#     if (length(digits) == 1)
#       x <- stp25stat2::render_f(x, digits = digits)
#
#     else if (!is.null(names(digits))) {
#
#       for (i in names(digits)) {
#         j <- which(names(x) == i)
#         x[[j]] <-
#           render_f(x[[j]], digits = digits[[i]])
#       }
#     }
#     else{
#       if(include.rownames) digits <- c(NA, digits)
#       print(digits)
#       for (i in seq_along(digits)) {
#         x[[i]] <-
#           render_f(x[[i]],
#                    digits = digits[i])
#       }
#     }
#   }
#   else if (auto.format) {
#     ptt_pval <- compleate_pattern(ptt_pval)
#     ptt_est <- compleate_pattern(ptt_est)
#     ptt_df <- compleate_pattern(ptt_df)
#     ptt_N <- compleate_pattern(ptt_N)
#
#     print(vars)
#     pval <- which(stringr::str_detect(vars, ptt_pval))
#     est <- which(stringr::str_detect(vars, ptt_est))
#     dgr <- which(stringr::str_detect(vars, ptt_df))
#     cnt <- which(stringr::str_detect(vars, ptt_N))
#     fstat <-
#       setdiff(seq_along(vars)[is_num], c(est, pval, dgr, cnt))
#
#
#     #  print(list(ptt_pval, pval))
#     if (length(cnt) > 0) {
#       x[cnt] <- render_f(x[cnt] , digits = 0)
#     }
#
#     if (length(pval) > 0) {
#       #  cat( "\nin pval\n")
#       for (i in pval) {
#         x[[i]] <-
#           rndr_P(x[[i]], include.symbol = FALSE)
#       }
#     }
#
#     if (length(est) > 0) {
#       x[est] <-
#         render_f(x[est], digits = get_opt("Fstat", "digits"))
#     }
#
#     if (length(fstat) > 0) {
#       x[fstat] <- render_f(x[fstat],
#                            digits = get_opt("Fstat", "digits"))
#     }
#
#     if (length(dgr) > 0) {
#       for (i in dgr) {
#         x[[i]] <-
#           render_f(x[[i]],
#                    digits = ifelse(any((x[[i]] %% 1 > 0)), 1, 0))
#       }
#     }
#   }
#
#   x
# }

#
# mod <- lm(
#   conformity ~ fcategory * partner.status,
#   data = Moore,
#   contrasts = list(fcategory = contr.sum, partner.status = contr.sum)
# )

# rslt <- Anova(mod)
#
#
# fix_format(rslt)
# fix_format(rslt, digits = c(2, 0, 2, 3))
# fix_format(rslt,  digits=c( "Pr(>F)"=4 ))





# fix_format <- function(x,
#                         digits = NULL,
#                         ptt_pval = c("Pr\\(\\>",
#                                      "Pr\\.\\.",
#                                      "p\\.value",
#                                      "p value",
#                                      "pvalue"),
#                         ptt_est = c("Estimate",
#                                     "Std\\. Error",
#                                     "est.std",
#                                     "se"),
#                         ptt_df = c("Df"),
#                         ptt_N = c("N","n"),
#
#                         ...) {
#   # if (!tibble::is_tibble(x))    x <- tibble::as_tibble(x)
#   is_num <- sapply(x, is.numeric)
#
#   vars <- tolower(names(x))
#   vars[which(!is_num)] <- "Character"
#
#
#   if (is.null(digits)) {
#     ptt_pval <- compleate_pattern(ptt_pval)
#     ptt_est <- compleate_pattern(ptt_est)
#     ptt_df <- compleate_pattern(ptt_df)
#     ptt_N <- compleate_pattern(ptt_N)
#
#
#     pval <- which(stringr::str_detect(vars, ptt_pval))
#     est <- which(stringr::str_detect(vars, ptt_est))
#     dgr <- which(stringr::str_detect(vars, ptt_df))
#     cnt <- which(stringr::str_detect(vars, ptt_N))
#     fstat <-
#       setdiff(seq_along(vars)[is_num], c(est, pval, dgr, cnt))
#
#     if (length(cnt) > 0) {
#       x[cnt] <- render_f(x[cnt] , digits = 0)
#     }
#
#     if (length(pval) > 0) {
#       for (i in pval) {
#         x[[i]] <-
#           rndr_P(x[[i]], include.symbol = FALSE)
#       }
#     }
#
#     if (length(est) > 0) {
#       x[est] <-
#         render_f(x[est], digits = get_opt("Fstat", "digits"))
#     }
#
#     if (length(fstat) > 0) {
#       x[fstat] <- render_f(x[fstat],
#                            digits = get_opt("Fstat", "digits"))
#     }
#
#     if (length(dgr) > 0) {
#       for (i in dgr) {
#         x[[i]] <-
#           render_f(x[[i]],
#                    digits = ifelse(any((x[[i]] %% 1 > 0)), 1, 0))
#       }
#     }
#   }
#   else{
#     if (length(digits) == 1)
#       x <- stp25stat2::render_f(x, digits = digits)
#     else if (!is.null(names(digits))) {
#       for (i in names(digits)) {
#         j <- which(names(x) == i)
#
#         x[[j]] <-
#           render_f(x[[j]], digits = digits[[i]])
#       }
#     }
#     else{
#       warning("Ich weis nicht wie ich die digits interpretieren soll?")
#     }
#   }
#   x
# }

# compleate_pattern <- function(x)
#   paste(paste0("^", x, "$"), collapse = "|")
#


