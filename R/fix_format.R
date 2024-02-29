#' @rdname render_f
#'
#' @param auto.format,pattern_pval,pattern_est,pattern_df,pattern_N  fix_format: pattern Sonderzeichen mit \\ schreiben!
#' @export
#'
#' @examples
#'
#' \donttest{
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
#'}
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



 #


