
#' @param x vector
#' @param n  rndr_percent: N
#' @param symbol.trailing  rndr_percent: Prozentzeichen
#' @param symbol.na  rndr_percent:
#' @param style  rndr_percent:
#' @param null_percent_sign  rndr_percent:
#' @param small_values  rndr_percent:
#'
#' @rdname render_f
#' @description rndr_percent:
#' style = get_opt("prozent", "style")
#'
#' 1 =>  "2.5 (2)"
#' 2 =>  "2 (2.5)"
#' 3 =>   "2.5"
#' 4 =>   "2"
#' 5 =>   "2/80"
#'
rndr_percent <- function(x = n / sum(n, na.rm = TRUE) * 100,
                         n = NULL,
                         digits = get_opt("prozent", "digits") ,
                         symbol.trailing = get_opt("prozent", "percentage_str"),
                         symbol.na = "n.a.",
                         style = get_opt("prozent", "style"),
                         null_percent_sign = get_opt("prozent", "null_percent_sign"),
                         small_values = x < 1 / (10 ^ digits)) {

  if (is.data.frame(x)) {
    x <-  as.matrix(x)
    n <-  as.matrix(n)
  }

  if (is.vector(x)) {
    if (!is.null(n))
      rndr_percent2(x,n,
                    digits,
                    symbol.trailing,
                    symbol.na,
                    style,
                    null_percent_sign,
                    small_values)
    else
      rndr_percent1(x,n,
                    digits,
                    symbol.trailing,
                    symbol.na,
                    style,
                    null_percent_sign,
                    small_values)

  }
  else if (inherits(x, "ftable") | is.table(x))  {
    save_dim <- dim(x)
    x <- as.matrix(x)
    n<- as.matrix(n)
    rslt <- if (!is.null(n))
            rndr_percent2(x, n,
                          digits,
                          symbol.trailing,
                          symbol.na,style,
                          null_percent_sign,
                          small_values)
            else
            rndr_percent1(x, n,
                          digits,
                          symbol.trailing,
                          symbol.na,style,
                          null_percent_sign,
                          small_values)

    if (length(save_dim) == 2) {
        stp25tools::fix_to_df(
          matrix(
            rslt,
            nrow = dim(x)[1],
            ncol = dim(x)[2],
            dimnames = dimnames(x)
          ),
          include.rownames = TRUE,
          include.dimnames = TRUE
        )
    }
    else{
      stp25tools::fix_to_df(
        matrix(
          rslt,
          dimnames = dimnames(x)
        ),
        include.rownames = TRUE,
        include.dimnames = FALSE
      )
    }
  }
  else if (is.matrix(x))  {
    if (!is.null(n))
      matrix(
        rndr_percent2(
          x,
          n,
          digits,
          symbol.trailing,
          symbol.na,
          style,
          null_percent_sign,
          small_values
        ),
        ncol = ncol(x),
        nrow = nrow(x),
        dimnames = dimnames(x)
      )
    else
      matrix(
        rndr_percent1(
          x,
          n,
          digits,
          symbol.trailing,
          symbol.na,
          style,
          null_percent_sign,
          small_values
        ),
        ncol = ncol(x),
        nrow = nrow(x),
        dimnames = dimnames(x)
      )
  }
  else{
    print(x)
    print(n)
    stop("\n class = ",class(x),"\n","dim = ",dim(x),"\n\n")
  }
}


# rndr_prct_ci

#' @examples
#' paste_prc(80, 2, 2 / 80 * 100, style = 1)
#' paste_prc(80, 2, 2 / 80 * 100, style = 2)
#' paste_prc(80, 2, 2 / 80 * 100, style = 3)
#' paste_prc(80, 2, 2 / 80 * 100, style = 4)
#' paste_prc(80, 2, 2 / 80 * 100, style = 5)
#'
#' @noRd
paste_prc <- function(n_total, n, prc, style = 1) {
  switch(
    style,
    "1" = paste(prc, " (", n, ")", sep = ""),
    "2" = paste(n, " (", prc, ")", sep = ""),
    "3" = prc,
    "4" = n,
    "5" = paste(n, "/", n_total, sep = ""),
    NA
  )
}


rndr_percent1 <- function(x,
                          n=NULL,
                          digits,
                          symbol.trailing,
                          symbol.na,
                          style,
                          null_percent_sign,
                          small_values) {
  prc <-
    render_f(
      x,
      digits = digits,
      drop0leading  = FALSE,
      format = "f",
      na.strings = NULL
    )
  if( all(is.na(x)) ) return(prc)

  if (any(small_values))
    prc[which(small_values)] <- paste0("<", 1 / (10 ^ digits))

  if (is.character(symbol.trailing))
    prc <-  paste(prc, symbol.trailing, sep = "")

  if (!is.null(null_percent_sign)) {
    prc[which(x == 0)] <- null_percent_sign
  }
  if (any(is.na(x)))
    prc[which(is.na(x))]  <-   symbol.na

  prc
}

rndr_percent2 <- function(x,
                          n,
                          digits,
                          symbol.trailing,
                          symbol.na,
                          style,
                          null_percent_sign,
                          small_values) {
  n_total <-
    sum(n, na.rm = TRUE)
  n <-
    render_f(
      n,
      digits = 0,
      drop0leading  = FALSE,
      format = "f",
      na.strings = NULL
    )
  prc <-
    rndr_percent1(x,
                  n,
                  digits,
                  symbol.trailing,
                  symbol.na,
                  style,
                  null_percent_sign,
                  small_values)

  rslt <- paste_prc(n_total, n, prc, style)

  if (any(is.na(x)))
    rslt[which(is.na(x))]  <-   symbol.na

  rslt
}


#
# -- Test --------------------------------
#
# rndr_percent(c(47.458, .154, 0, NA))
# # rndr_percent(47.458, 154)
# rndr_percent(c(12.148, 0.0266, 2, 0, 36.478, NA),
#              c(148, 1, 14, 0, 154, NA),
#              digits = 1)
# rndr_percent(
#   c(12.148, 0.0266, 2, 0, 36.478),
#   c(148, 1, 14, 0, 154),
#   digits = 1,
#   style = 5
# )
#
# rndr_percent(data.frame(
#   p = c(1, 2, 3),
#   p2 = c(10, 0, 36),
#   p3 = c(1.1, 2.1, NA)
# ) ,
# data.frame(
#   n = c(100, 200, 300),
#   n1 = c(1000, 0, 360),
#   n2 = c(11, 21, NA)
# ))
#
#
#
# n <- 100
# lvs <- c("--", "-", "o", "+", "++")
# DF2 <- data.frame(
#   Magazines = cut(rnorm(n), 5, lvs),
#   Comic.books = cut(rnorm(n), 5, lvs),
#   Fiction = cut(rnorm(n), 5, lvs),
#   Newspapers = cut(rnorm(n), 5, lvs),
#   Geschlecht = cut(rnorm(n), 3, c("m", "f", "d"))
# )
# x <- with(DF2, table(Magazines, Geschlecht))
#
# cnt <- ftable(x)
# prop_table <- prop.table(x)
# prop_table[which(is.na(prop_table))] <- 0
# prc <-  ftable(prop_table * 100)
# rndr_percent(prc, cnt)
#
# x <- with(DF2, table(Magazines, Geschlecht))
# prop_table <- prop.table(x)
#
# rndr_percent(prop_table, x)



