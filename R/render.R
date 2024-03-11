#' Format a vector of provided numeric values
#'
#' Given a vector of data return as strings formatted as requested
#'
#'
#' Stolen from tangram
#'
#' @param x numeric; the data to format. Must work with quantile function.
#' @param digits the decimal
#' @param drop0leading leading zero
#' @param na.strings,na.symbol NA replace
#' @param decimal.mark an formatC default format="f"
#' @param ... weitere Argumente an formatC  drop0leading,format,decimal.mark
#'
#' @return character; formatted values as character strings
#' @export
#' @examples
#'
#'  \donttest{
#'  # Vector
#' x <- c(1, 0, 2.45896, 2548.256)
#' y <- c(0.1, 0.01 , 0.001, NA)
#' z <- c(1L, 2L, 3L)
#' render_f(x)
#' render_f(y)
#' render_f(z)
#' render_f(as.double(x))
#' render_f(letters[1:3])
#'
#' render_f(x, c(1, 2, 3, 4))
#' render_f(x, c(1, 2, 3))
#'
#' # Data.Frame
#' dat <- data.frame(Item = letters[1:4], x = x, y = y)
#' render_f(dat)
#' render_f(dat, digits = 2)
#' render_f(dat, digits = c(1, 2))
#' render_f(dat, digits = c(1, 2, 3, 4, 1))
#' render_f(dat,
#'          digits = list(c(1, 2), c(3)),
#'          drop0leading = list(FALSE, TRUE))
#'
#' # Matrix
#' render_f(cbind(y, x))
#' render_f(cbind(y, x) , digits = list(3, 0))
#' # List
#' mx <- list(x = x, y = y, z = rnorm(10))
#' render_f(mx)
#' render_f(mx, digits = 2)
#' render_f(mx, digits = list(2, 0, 1))
#'
#'}
render_f <- function(x, digits = NULL, ...) {
  UseMethod("render_f")
}


#' @rdname render_f
#' @export
render_f.list <- function(x, digits = NULL, ...) {
  if (!is.list(digits)) {
    lapply(x,  render_f, digits=digits, ...)
  }
  else {
    purrr::pmap(list(x,
                     digits = digits,
                     ...),
                render_f)
  }
}


#' @rdname render_f
#' @export
render_f.matrix <- function(x, digits = NULL, ...) {
  if (!is.list(digits)) {
    apply(x, 2, render_f, digits=digits, ...)
  }
  else {
    as.matrix(data.frame(purrr::pmap(
      list(data.frame(x),
           digits = digits,
           ...),
      render_f
    )))
  }
}


#' @rdname render_f
#' @export
render_f.data.frame  <- function(x, digits = NULL, ...) {
  which_is_num <- sapply(x, is.numeric)
  if (!is.list(digits)) {
    x[which_is_num] <-
      apply(x[which_is_num], 2, render_f, digits = digits, ...)
  }
  else {
    x[which_is_num] <-as.data.frame(
        purrr::pmap(
          list(x[which_is_num],
          digits = digits, ...),
          render_f))
  }
  x
}


#' @rdname render_f
#' @export
render_f.tbl_df  <- function(x, digits = NULL, ...) {
  which_is_num <- sapply(x, is.numeric)
  if (!is.list(digits)) {
    x[which_is_num]  <-
      tibble::as_tibble(lapply(x[which_is_num],
                               render_f , digits = digits, ...))
  }
  else {
    x[which_is_num] <-
      tibble::as_tibble(purrr::pmap(
        list(x[which_is_num],
             digits = digits, ...),
         render_f))
  }
  x
}


#' @rdname render_f
#' @export
render_f.numeric <- function(x, digits = NULL, ...) {
  if (is.null(digits))
    make_format(x, ...)
  else if (length(digits) == 1)
    make_format(x, digits=digits, ...)
  else
    mapply(make_format, x,
           digits=length_multiplier(digits, length(x)), ...)
}


#' @rdname render_f
#' @export
render_f.default <- function(x,
                             digits= NULL,
                             drop0leading = FALSE,
                             na.strings = "NA",
                             na.symbol = "",
                             decimal.mark = OutDec(),

                             ...) {
  x <- as.character(x)
  if (!is.null(na.strings))
    x[stringr::str_detect(x, na.strings)] <- na.symbol

  if (drop0leading)
    x <- drop_0_leading(x, decimal.mark)

  x
}

# helper ------------------------------------------------------------------

OutDec <- function() {
  if(is.null( get_opt("OutDec") ))  "."
  else get_opt("OutDec")

}

#' @noRd
make_format <- function(x,
                        digits = format_guess(x),
                        drop0leading  = FALSE,
                        format = "f",
                        decimal.mark = OutDec(),
                        na.strings = "NA",
                        na.symbol = "",
                        unit = NULL,
                        ...) {
  if (length(x) == 1)
    if (is.na(x))
      return(na.symbol)
  x <- formatC(x,
               digits = digits,
               format = format,
               decimal.mark = decimal.mark,
               ...)



  if (!is.null(na.strings))
    x[stringr::str_detect(x, na.strings)] <- na.symbol

  if (drop0leading)
    x <- drop_0_leading(x, decimal.mark)

  x
}


#' Laenge der digits anpassen
#'
#' @noRd
length_multiplier <- function(x,
                              n_out = NULL,
                              n  = length(x)) {
  if (n == n_out)
    x
  else if (n == 1)
    rep(x, n_out)
  else if (n > n_out)
    x[1:n_out]
  else if (n < n_out)
    c(x, rep(x[n], n_out - n))
  else
    NULL
}




# Guess the best format for a given set of numerical data
#
# Given a vector of data, default to 3 significant digits or all if maximum is greater
# than zero
#
# @param x numeric; basic math and quantile function must work on data passed in
# @return numeric; the digits past the decimal recommended for display
# @examples
#   stp25stat2:::format_guess(rnorm(100))
#   stp25stat2:::format_guess(rnorm(100, sd=1e-6))
#' @noRd
format_guess <- function(x) {

  d <- x[!is.na(x)]
  if (length(d) == 0)
    return(0) # Nothing, then just return 0 for rounding
  if (all(d == floor(d)))
    # Is it all whole numbers, then no decimals
    0
  else
  {
    consider <- quantile(abs(d), c(0.05, 0.5))
    if (sum(consider) == 0.0)
      3
    else
      # Otherwise use 3 significant digits of a representative smaller side quantile
      max(2 - max(floor(log10(consider))), 0)
  }
}


#' Fuerende Null eliminieren
#'
#' @noRd
drop_0_leading <- function(x,
                           OutDec = OutDec()) {
  sub(glue::glue('^(-)?0[{OutDec}]'),
      glue::glue('\\1{OutDec}'),
      x)

}


# p-Value -----------------------------------------------------------------


rndr_P <-
  function(x,
           include.symbol = TRUE,
           digits = get_opt("p", "digits"),
           with.stars = get_opt("p", "with.stars"),
           symbol.leading = c("p=", "p<"),
           mark.sig = get_opt("p", "mark.sig"),
           ...) {

    is_na<-is.na(x)
    p <- render_f(
      x,
      digits = digits,
      format = "f",
      drop0leading = TRUE
    )


    if (include.symbol)
      p <- ifelse(p == ".000",
                  paste0(symbol.leading[2], ".001"),
                  paste0(symbol.leading[1], p))
    else
      p <- ifelse(p == ".000", "<.001", p)


    if (with.stars)
      p <- paste0(p, rndr_Stars(x))

    if (!is.null(mark.sig))
      p <- add_marking(x, p)

    if(any(is_na)) p[is_na] <- ""

    p
  }

add_marking <- function(x, p,
                        symbol.leading =
                          if (which_output() == "html") "<b>" else " # ",
                        symbol.trailing = if (which_output() == "html") "</b>"else " # ") {
  ifelse(x <= .050,    paste0(symbol.leading, p, symbol.trailing), p)
}

rndr_Stars <- function (x,
                        stars.value =  get_opt("p", "stars.value"),
                        stars.symbols = get_opt("p", "stars.symbols")){
  p_sternchen <- function(x)  {
    stern <- as.character(cut(round(x, 3),
                              c(-Inf, stars.value, Inf),
                              c(stars.symbols, "")))
    stern[is.na(stern)] <- ""
    stern
  }

  if (is.vector(x)) {
    xnames <- names(x)
    x <- p_sternchen(x)
    names(x) <- xnames
  }
  else if (is.data.frame(x)) {
    xnames <- names(x)
    x <- data.frame(lapply(x, p_sternchen),
                    stringsAsFactors = FALSE)
    names(x) <- xnames
  }
  else if (is.matrix(x)) {
    xnames <- dimnames(x)
    x <- apply(x, 2, p_sternchen)
    dimnames(x) <- xnames

  }

  x
}






# mean --------------------------------------------------------------------

#' render_mean
#' Formatiere von Zahlen nach dem APA-Style ( American Psychological Association )
#' @noRd
rndr_median_iqr <- function(m,
                        iqr,
                        digits = get_opt("median", "digits"),
                        sep =    get_opt("median", "seperator"),
                        style =  get_opt("median", "style"),
                        ...) {

  paste0(render_f(m, digits, ...),
         " (IQR ",
         render_f(iqr, digits, ...),
         ")")
}



rndr_median_quant <- function(x,
                              digits = get_opt("median", "digits"),
                              sep =    get_opt("median", "seperator"),
                              style =  get_opt("median", "style"),
                              ...) {
  paste0(
    render_f(x[3], digits, ...),
    " (",
    render_f(x[2], digits, ...),
    sep,
    render_f(x[4], digits, ...),
    ")"
  )
}

rndr_median<-
rndr_median_iqr_range <- function (m,
                               iqr,
                               mn,
                               mx,
                               digits = get_opt("median", "digits"),
                               sep = get_opt("median", "seperator"),
                               ...) {
  paste0(
    render_f(m, digits, ...),
    " (IQR ",
    render_f(iqr, digits, ...),
    sep,
    "range ",
    render_f(mn, digits, ...),
    " to ",
    render_f(mx, digits, ...),
    ")"
  )

}

rndr_median_range <-
function(x,
         digits = get_opt("median", "digits"),
         sep =    get_opt("median", "seperator"),
         style =  get_opt("median", "style"),
         ...) {
  paste0(
    render_f(x[3], digits, ...),
    " (",
    render_f(x[1], digits, ...),
    sep,
    render_f(x[5], digits, ...),
    ")"
  )
}


rndr_mean <- function(m,
                      s,
                      digits = get_opt("mean", "digits"),
                      style = get_opt("mean", "style"),
                      ...) {

  # if (is.null(style))
  #   style <- get_opt("mittelwert", "mean.style")

  if(style == 1 )
  paste0(render_f(m, digits, ...),
         " (", render_f(s, digits, ...), ")")
  else
    paste0(render_f(m, digits, ...),
           get_opt("mean", "plusmin_str"),
           render_f(s, digits, ...))

}

rndr_mean_range <- function(m,
                            s,
                            mn,
                            mx,
                            digits = get_opt("mean", "digits"),
                            line_break = get_opt("mean", "seperator"),
                            style = get_opt("mean", "style"),
                            ...) {
# noch nicht implementiert
#  if (is.null(style)) style <- get_opt("mittelwert", "mean.style")

  paste0(
    render_f(m, digits, ...),
    " (SD ",
    render_f(s, digits, ...),
    line_break,
    "range ",
    render_f(mn, digits, ...),
    " to ",
    render_f(mx, digits, ...),
    ")"
  )

}


rndr_ods <- function(x, digits = get_opt("r", "digits")) {
  res <- render_f(x, digits = digits)
  res[which(x > 20)] <- ">20"
  res[which(x < .01)] <- "<0.01"
  res
}


rndr_CI <- function(ci,
                    digits = get_opt("r", "digits"),
                    sep =    get_opt("sep_element"),
                    sep_1 =  get_opt("brackets")[1],
                    sep_2 =  get_opt("brackets")[2]) {
  if (is.vector(ci)) {
    paste0(sep_1,
           render_f(ci[1], digits),
           sep,
           " ",
           render_f(ci[2], digits),
           sep_2)

  }
  else {
    paste0(sep_1,
           render_f(ci[, 1], digits),
           sep,
           " ",
           render_f(ci[, 2], digits),
           sep_2)
  }

}


rndr_CI2 <-  function(ci,
                      digits = 3,
                      sep =   ",",
                      sep_1 =  "[",
                      sep_2 =  "]",
                      format = "g") {
  cis <-  paste0(
    sep_1,
    render_f(ci[[1]],
                     digits, format = format),
    sep,
    " ",
    render_f(ci[[2]],
                     digits, format = format),
    sep_2
  )

  cis[is.na(ci[[1]])] <- NA
  cis
}


rndr_ods_CI <- function(ci,
                        digits =  get_opt("r", "digits"),
                        sep =   get_opt("sep_element"),
                        sep_1 = get_opt("brackets")[1],
                        sep_2 = get_opt("brackets")[2]) {
paste0(sep_1,
                rndr_ods(ci[, 1], digits),
                sep,
                " ",
                rndr_ods(ci[, 2], digits),
                sep_2)


}


rndr_mean_CI <- function(m, ci, digits = get_opt("r", "digits" ), ...) {
  m<- render_f(m, digits)
 ci <-  rndr_CI(ci, digits)

# print(m)
# print(ci)
  paste( m, ci )


}


rndr_r <- function(x, include.symbol = TRUE,
                   digits = get_opt("r", "digits" ),
                   drop0leading = !get_opt("r",  "lead.zero" ),
                   ...) {
 r2<- render_f(x,
           digits = digits,
           drop0leading  = drop0leading,
           ...)

  if (include.symbol)  paste0("r=", r2)
  else  r2
}

rndr_r2 <- function(x, include.symbol = TRUE, ...) {
  r2 <-  rndr_Effect_Size(x, ...)
  if (include.symbol) {
    paste(paste0(c(
      "R<sup>2</sup>", "adj.R<sup>2</sup>"
    ), "=",
    r2),
    collapse = ", ")
  }
  else
    r2
}


rndr_r2pseudo <- function(x, include.symbol = TRUE, ...) {
  r2 <- rndr_Effect_Size(x, ...)
  if (include.symbol)
    paste(paste0(names(r2), "=", r2), collapse = ", ")
  else
    r2
}


rndr_corr <- function(x, p, df) {
  paste0("r", rndr_df(df), "=", rndr_Effect_Size(x), ", ", rndr_P(p))
}


rndr_Effect_Size <- function(x,
                             digits = get_opt("r", "digits" )[1],
                             drop0leading = !get_opt("r",  "lead.zero" ),
                             ...) {
          render_f(x,
          digits = digits,
          drop0leading  = drop0leading,
          ...)


}



# Tests -------------------------------------------------------------------


rndr_Test_Statistic <-
  function (x,
            digits = get_opt("Fstat", "digits") ,
            drop0leading = !get_opt("Fstat", "lead.zero"),
            ...) {
    render_f(x,
             digits = digits,
             drop0leading  = drop0leading,
             ...)
  }

rndr_df <- function(df1, df2 = NULL) {
  sub <- sub_tiefgestellt()
  if (is.null(df2))
    paste0(sub[1], "(", render_f(df1, 0), ")", sub[2])
  else
    paste0(sub[1], "(", render_f(df1, 0), ", ", render_f(df2, 0), ")", sub[2])
}


rndr_F <-
  function(F_val,
           df1,
           df2,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      F_val <-
        paste0("F", rndr_df(df1, df2), "=", rndr_Test_Statistic(F_val))
      if (is.null(p))
        F_val
      else
        paste0(F_val, sep, rndr_P(p))
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_T <-
  function(F_val,
           df1,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      F_val <- paste0("T", rndr_df(df1), "=", rndr_Test_Statistic(F_val))
      if (is.null(p))
        F_val
      else
        paste0(F_val, sep, rndr_P(p))
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_H <-
  function(F_val,
           df1,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      F_val <- paste0("H", rndr_df(df1), "=", rndr_Test_Statistic(F_val))
      if (is.null(p))
        F_val
      else
        paste0(F_val, sep, rndr_P(p))
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_BP <-
  function(F_val,
           df1,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      F_val <- paste0("BP", rndr_df(df1), "=", rndr_Test_Statistic(F_val))
      if (is.null(p))
        F_val
      else
        paste0(F_val, sep, rndr_P(p))
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_DW <-
  function(F_val,
           df1,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      F_val <- paste0("DW",
                      "=", rndr_Test_Statistic(F_val))
      if (is.null(p))
        F_val
      else
        paste0(F_val, sep, rndr_P(p))
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_W <-
  function(F_val,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      F_val <- paste0("W=", rndr_Test_Statistic(F_val))
      if (is.null(p))
        F_val
      else
        paste0(F_val, sep, rndr_P(p))
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_U <-
  function(F_val,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      F_val <- paste0("U=", rndr_Test_Statistic(F_val))
      if (is.null(p))
        F_val
      else
        paste0(F_val, sep, rndr_P(p))
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_shapiro <-
  function(F_val,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      F_val <- paste0("W=", rndr_Test_Statistic(F_val))
      if (is.null(p))
        F_val
      else
        paste0(F_val, sep, rndr_P(p))
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_lm <-
  function(F_val,
           df1,
           df2,
           p,
           r2,
           ar2,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic)
      paste0(
        "R2=",
        rndr_Effect_Size(r2),
        sep,
        "ad.R2=",
        rndr_Effect_Size(ar2),
        sep,
        rndr_F(F_val, df1, df2, p)
      )
    else
      rndr_P(p, FALSE)

  }


rndr_X <-
  function(x,
           df1,
           df2 = NULL,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      if (is.null(df2)) {
        if (!is.null(df1))
          x <-
            paste0(symbol_chi2(),
                   rndr_df(df1),
                   "=",
                   rndr_Test_Statistic(x))
        else
          x <- paste0(symbol_chi2(), "=", rndr_Test_Statistic(x))
      } else {
        x <-
          paste0(symbol_chi2(),
                 rndr_df(df1),
                 "=",
                 rndr_Test_Statistic(x))
      }
      if (!is.null(p))
        paste0(x, sep, rndr_P(p))
      else
        x
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_Chisq <- function(x, df, p)
  rndr_X(x, df, NULL, p)


rndr_Chisq_stars <-
  function(x, p) {
    paste0(rndr_Test_Statistic(x) , rndr_Stars(p))
  }


rndr_fischer <-
  function(x,
           p,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic)
      paste0("OR=", rndr_Test_Statistic(x), sep, rndr_P(p))
    else
      rndr_P(p, FALSE)
  }



# Symbols -----------------------------------------------------------------

symbol_nbsp <-   function(output = which_output()) {
  switch(
    output,
    html = "&nbsp;",
    " ")

}

symbol_chi2 <- function(output = which_output()) {
  switch(
    output,
    text =  "X2",
    markdown =  "X2",
    html =  "&chi;<sup>2</sup>",
    word =   "X2",
    "X2"
  )
}

symbol_kleiner_gleich <- function(output = which_output()) {
    if (output == "html")
      "&le;"
    else
      "=<"
  }

symbol_groesser_gleich <-  function(output = which_output()) {
    switch(
      output,
      html = "&ge;",
      "=>")
  }

symbol_alpha	<- function(output = which_output()) {
  switch(
    output,
    html = "&alpha;",
    "alpha")

}

symbol_beta	<- function(output = which_output()) {
  switch(
    output,
    html = "&beta;",
    "beta")
}

symbol_eta	<-  function(output = which_output()) {
    switch(
      output,
      html = "&eta;",
      "eta")
  }

symbol_kappa	<- function(output = which_output()) {
  switch(
    output,
    text =  "kappa",
    markdown =  "kappa",
    html =  "&kappa;",
    word =   "kappa",
    "kappa"
  )
}



#sep <- get_opt("sep_element")

sub_tiefgestellt <- function(output = which_output()){
  switch(
    output,
    text =  c("", ""),
    markdown =   c("", ""),
    html =  c("<sub>", "</sub>"),
    word =  c("", ""),
    c("", "")
  )

}



