fix_and_format<- function(x, caption, digits=2, ...) {
  prepare_output(
    fix_format(
      stp25tools::fix_to_df(x),
      digits=digits),
    caption=caption
  )
}


#' @rdname extract
#'
#' @param fit_predict an MASS predict.lda
#' @param newdata model.frame(x)
#' @param digits nur fuer die means
#' @param ... nicht benztzt
#'
#' @return
#' @export
#'
#' @examples
#'
#'  require(MASS)
#' DF2 <- stp25aggregate::GetData(
#'   "C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25data/extdata/discrim.sav")
#' #--https://stats.idre.ucla.edu/spss/dae/discriminant-function-analysis/
#' DF2$Job <- factor(DF2$JOB, 1:3, Cs("customer service", "mechanic","dispatcher"))
#' DF2$Job2 <- factor(DF2$JOB, c(2,3,1), Cs( "mechanic","dispatcher","customer service"))
#'
#' fit2 <- lda(Job ~ OUTDOOR+SOCIAL+CONSERVATIVE, data=DF2)
#' fit3 <- lda(Job2 ~ OUTDOOR+SOCIAL+CONSERVATIVE, data=DF2)
#'
#' tbll_extract.lda(fit2)
#' tbll_extract.lda(fit3)
#'
#'
tbll_extract.lda <-
  function(x,
           fit_predict = MASS:::predict.lda(x),
           newdata = model.frame(x),
           digits=3,
           ...) {
    means <- fix_and_format(t(x$means),
                            caption = "Means", digits=digits)

    scaling <-  stp25tools::fix_to_df(x$scaling)
    scaling <- prepare_output(
      fix_format(scaling[order(-scaling[, 2]), ]),
      caption = "Coefficients of linear discriminants")
    svd <- x$svd
    names(svd) <- dimnames(x$scaling)[[2L]]
    if (length(svd) > 1L) {
      svd <- prepare_output(
        fix_format(
          data.frame(t(data.frame(
            svd ^ 2 / sum(svd ^ 2)
          )))),
        caption = "Proportion of trace")
    }

    cTab1 <- table(newdata[, 1], fit_predict$class,
                   dnn = c(names(newdata)[1], "Predict"))

    cTab <- fix_and_format(
      addmargins(cTab1),
      caption = "Kontingenztafel tatsaechlicher und vorhergesagter Kategorien",
      digits=0)


    cTotal <- c(diag(prop.table(cTab1, 1)),
                Total = sum(diag(prop.table(cTab1)))) * 100
    cTotal <-
      prepare_output(
        data.frame(Item = names(cTotal),
                   percent = render_f(cTotal, digits=1)),
        caption = "prozentuale Uebereinstimmung")

    names(cTotal)[1] <- names(cTab)[1]

    list(
      mean = means,
      scal = scaling,
      svd =  svd,
      cTab = cTab,
      cTotal = cTotal
    )
  }


