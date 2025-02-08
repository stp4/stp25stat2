
#' fix_format
#'
#' @description
#' Formatieren der Zahlen für die Ausgabe mit Output()
#'
#' fix_format():
#' Alle numeric werden entweder geraten oder mit einer fixen
#' Nachkomastelle formatiert.
#'
#' fix_format2(): Erratet die p-Werte und formatiert
#'
#'
#' @param x an R object. data.frame or list
#' @param digits integer. if Null stp25stat2:::format_guess()
#'  a positive integer indicating how many significant digits are to be used
#' @param names_repair logical. P-value and SE
#' @param include.rownames logical. TRUE
#' @param p.value character.  pattern for p_values
#' @param se character. pattern for Estimate and SE
#' @param df character. pattern for degree of fredom
#'
#' @export
#'
#' @importFrom stringr str_starts
#' @importFrom purrr map2
#' @importFrom dplyr bind_cols
#' @importFrom stp25tools fix_to_tibble
#'
#' @returns A tbble data.frame the same length as the input.
#'
#'
#' @examples
#'
#' df2 <-
#'   data.frame(
#'     term = c("A", "B", "C", "D"),
#'     Estimate = c(23.5567, .148990, 5.643256, 2.9876),
#'     df1 = c(3.3, 35., 7.8, 2.1),
#'     df = c(3, 35, 7, 2),
#'     N = c(33, 35, 78, 21),
#'     F.value = c(2.73345, 12.4446, 14.5767, 30.4128),
#'     pvalue = c(0.73123, 0.044456, 0.056789, 0.042654),
#'     stringsAsFactors =FALSE
#'   )
#'
#' # Erratet die p-Wert
#' fix_format(df2)
#'
#' # versucht zu eraten wie viele digits
#' fix_format2(df2)
#'
#' # fest vorgegeben
#' fix_format2(df2, digits = 2)
#'
#'
#'
#'
#' #  require(car)
#' # mod1 <- lm(conformity ~ fcategory * partner.status, data = Moore)
#' # mod2 <-
#' #   glm(conformity ~ fcategory * partner.status,
#' #       data = Moore,
#' #       family = poisson())
#' #
#' # Anova(mod1) |> fix_format()
#' # summary(mod1)$coefficients |> fix_format()
#' # Anova(mod2) |> fix_format()
#' # summary(mod2)$coefficients |> fix_format()
#'
fix_format <- function(x, ...) {
  UseMethod("fix_format")
}



#' @export
fix_format.default <- function(x,
                               digits = NULL,
                               names_repair = TRUE,
                               include.rownames = TRUE,
                               p.value = c(
                                 "Pr\\(\\>",
                                 "p value",
                                 "p.value",
                                 "pvalue"),
                               se = c("Std\\. Error", "est.std"),
                               df = c("N", "Df")) {
  x <-
    stp25tools::fix_to_tibble(
      x,
      include.rownames = include.rownames)
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
    # digits[which(detect_df)] <- "integer"
  }


  # purrr::map2_dfc(x, digits,
  #                 function(x, digits) {
  #                   if (is.na(digits))
  #                     x
  #                   else if (is.numeric(digits))
  #                     render_f(x, digits)
  #                   else if (digits == "pvalue")
  #                     rndr_P(x, include.symbol = FALSE)
  #                   else if (digits == "numeric")
  #                     render_f(x, 2)
  #                   else
  #                     render_f(x, 0)
  #                 })
  #


  res <- purrr::map2(x, digits,
                  function(x, digits) {
                    if (is.na(digits))
                      x
                    else if (is.numeric(digits))
                      render_f(x, digits)
                    else if (digits == "pvalue")
                      rndr_P(x, include.symbol = FALSE)
                    else if (digits == "numeric")
                      render_f(x, NULL)
                    else
                      x #render_f(x, 0)
                  })

  dplyr::bind_cols(res)


}

#' @export
fix_format.list <-
  function(x, ...) {
    rslt <- NULL
    for (i in names(x)) {
      if (is.data.frame(x[[i]]))
        rslt[[i]] <- fix_format(x[[i]], ...)
    }
    if (length(rslt) == 1)
      rslt <- rslt[[1]]
    else if (length(rslt) == 0)
      rslt <- "Keine Ahnung was ich das machen soll? Ich kann nur data.frames aufdroeseln!"

    rslt
  }



#' @rdname fix_format
#' @export
fix_format2 <-  function(x,
                         digits = NULL,
                         include.rownames = TRUE) {
  x <-
    stp25tools::fix_to_tibble(x,
                              include.rownames = include.rownames)

  stp25tools:::dapply1(x, function(xx) {
    if (is.numeric(xx))
      render_f(xx, digits)
    else
      xx
  })
}
