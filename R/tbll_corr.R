#' @param include.stars,include.p Tbll_corr(): P-Werte,Explizite p-Werte, Sternchen als p-Werte
#' @param cor_diagonale_up Tbll_corr(): Diagonale oben oder unter
#' @param type Tbll_corr(): enweder "pearson" oder "spearman"
#' @rdname Tbll_desc
#' @export
#' @examples
#'
#'
#'  \donttest{
#' # install.packages("corrr")
#' require(corrr)
#' require(stp25stat2)
#' require(stp25tools)
#' set.seed(1)
#' n <- 2 * 20
#' x <- rnorm(n)
#' y <- rnorm(n)
#'
#' dat <-
#'   data.frame(
#'     group = gl(2, 20, labels = c("Control", "Treat")),
#'     a = rnorm(n,0,1),
#'     b =  x,
#'     c =  y,
#'     d = rnorm(n,0,.5) + x + y,
#'     e = rnorm(n,0,.5) + x - y,
#'     f = rnorm(n,0,.5) - x + y,
#'     g = rnorm(n,0,.75)  + x,
#'     h = rnorm(n,0,.5) - y
#'   ) |> Label(
#'     a = "Alpha",
#'     b = "Beta",
#'     c = "Gamma"
#'   )
#'
#'
#' Tbll_corr( ~ a + b + c, dat)
#' Tbll_corr(a ~ c, dat)
#' Tbll_corr(a + b + c ~ d, dat)
#' Tbll_corr(a + b + c ~ d, dat, groups = ~ group)
#'
#' x<-corrr::correlate(dat[-1])
#' x
#' #corrr::fashion(x)
#' #corrr::rplot(x)
#' corrr::network_plot(x, min_cor = .2)
#'
#' }
Tbll_corr <-
  function(...,
           include.label=TRUE,
           include.mean = FALSE,
           include.n = TRUE,
           include.stars = TRUE,
           include.p = FALSE,
           cor_diagonale_up = TRUE,
           type = c("pearson", "spearman")) {
    Hmisc_rcorr(
      ...,
      cor_diagonale_up = cor_diagonale_up,
      include.stars = include.stars,
      include.p  = include.p,
      include.mean = include.mean,
      include.n = include.n,
      type = type
    )
  }


Hmisc_rcorr <- function(...,
                        cor_diagonale_up = TRUE,
                        include.stars = TRUE,
                        include.p  = FALSE,
                        include.mean = FALSE,
                        include.n = TRUE,
                        type = c("pearson", "spearman")
                      ) {
  X <- stp25tools::prepare_data2(...)

  if( is.null(X$data) ) return(Info_Statistic("Correlation","Hmisc", "rcorr"))

  type <-  match.arg(type)
  condition <- X$condition.vars
  measure.vars <- X$measure.vars
  measure_data <- as.matrix(sapply(X$data[measure.vars], as.numeric))
  group.vars <- X$group.vars
  N <- nrow(X$data[measure.vars])
  ans <- NULL

  if (!is.null(condition)) {# a + b ~ c | d

    if (length(condition) < 1)
      warning("kann nur eine Gruppe aufdroeseln!")
    condition <- X$data[[condition[1]]]

    if (!is.factor(condition)) {
      warning("Achtung nur eine Faktor kann Gruppen bilden!")
      return(head(data))
    }

    group_data <- as.matrix(sapply(X$data[group.vars], as.numeric))

    condition <- droplevels(condition)
    lvls <- levels(condition)


    for (i in seq_len(length(lvls))) {
      g2 <- which(condition == lvls[i])

      ans_corr <-
        Hmisc::rcorr(measure_data[g2, ], group_data[g2, ], type = type)
      k <- ncol(ans_corr$r)
      r <- ans_corr$r[-k, k]
      p <- ans_corr$P[-k, k]

      ans_corr <- data.frame(
        Characteristics = X$row_name ,
        N = round(ans_corr$n[-k, k], 0),
        r =  rndr_r(r, FALSE),
        p.value = rndr_P(p, FALSE),
        stringsAsFactors = FALSE
      )

      if(!include.n) ans_corr <- ans_corr[-2]
      names(ans_corr)[-1] <-
        paste0(lvls[i], "_", names(ans_corr)[-1])
      if (i == 1) {
        ans <- ans_corr
      }
      else{
        ans <- cbind(ans, ans_corr[-1])
      }
    }
  }
  else if (!is.null(group.vars)) {  #   a + b ~ c + d
   group_data <- as.matrix(sapply(X$data[group.vars], as.numeric))

    for (i in 1:(length(group.vars))) {
      ans_corr <- Hmisc::rcorr(measure_data,
                               group_data[, i],
                               type = type)
      k <- ncol(ans_corr$r)
      r <- ans_corr$r[-k, k]
      p <- ans_corr$P[-k, k]

      ans_corr <- data.frame(
        Characteristics = X$row_name ,
        N = round(ans_corr$n[-k, k], 0),
        r =  rndr_r(r, FALSE),
        p.value = rndr_P(p, FALSE),
        stringsAsFactors = FALSE
      )

      if(!include.n) ans_corr <- ans_corr[-2]
      names(ans_corr)[-1] <-
        paste0(group.vars[i], "_", names(ans_corr)[-1])
      if (i == 1) {
        ans <- ans_corr
      }
      else{
        ans <- cbind(ans, ans_corr[-1])
      }
    }
  }
  else{ # Korrelations_Matrix  ~ a + b + c
    ans_list <- Hmisc::rcorr(measure_data,
                             type = type)
    r <- rndr_r(ans_list$r, FALSE)

    if (include.stars & !include.p) {
      r <- matrix(
        paste0( r , ", ", rndr_Stars(ans_list$P)),
        nrow = nrow(ans_list$r),
        dimnames = dimnames(ans_list$r)
      )
    } else if (include.p) {
      r <- matrix(
        paste0( r , ", ", rndr_P(ans_list$P, TRUE, with.stars = include.stars)),
        nrow = nrow(ans_list$r),
        dimnames = dimnames(ans_list$r)
      )
    } else { NULL }
    r <- format_diagonale(r, cor_diagonale_up)
    ans <- data.frame(Source = rownames(ans_list$r),
                       r,
                       stringsAsFactors = FALSE)
    # Labels
    my_num <- paste0("(", 1:length(X$row_name), ")")
    ans[, 1] <-   paste(my_num, X$row_name)
    colnames(ans)[2:ncol(ans)] <- my_num

    if (include.mean) {
      ans_mean <-
        t(
          berechne_all(
            X$data[measure.vars],
            X$measure.vars,
            measure = "mean",
            type = 1,
            measure.name = "value"
          )
        )


      ans <- cbind(ans[1],
                   "M (SD)" = ans_mean,
                   ans[2:ncol(ans)], stringsAsFactors=FALSE)
    }
  }

  prepare_output(ans,
                 caption="Correlations",
                 note= type,
                 N=N)
}


format_diagonale <- function(mycorrtable,
                             cor_diagonale_up,
                             d = 1,
                             l = "") {
  diag(mycorrtable) <- d
  if (cor_diagonale_up)
    mycorrtable[lower.tri(mycorrtable)] <- l
  else
    mycorrtable[upper.tri(mycorrtable)] <- l

  mycorrtable
}
