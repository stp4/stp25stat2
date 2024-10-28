#' Tbll_lda
#'
#' Linear Discriminant Analysis
#'
#' @export
#' @param x lda {MASS}
#' @param newdata  Test daten wenn Null dann wird model.frame(x) verwendet und an MASS::predict.lda weitergegeben.
#' @param digits digits
#' @param include.means aritmetic means
#' @param include.scal scaling (LD)
#' @param include.cTab Accuracy Table
#' @param include.svd Eigenvalue
#' @param ...
#'
#' @return list with means, scal, cTab, include.svd and predictiv-Data
#'
#' @examples
#'
#' # Linear Discriminant Analysis
#'  require(MASS)
Tbll_lda <- function(..., include.label = NULL){

  if(is.null(include.label)){
     return(tbll_extract.lda(...))
    }
  else {
    if (is_logical(include.label)) stop("\nHier musst du mir die Labels als Character-String uebergeben.\n")
    else{
      rst<- tbll_extract.lda(...)
      pos <- which(names(include.labels)  %in% rst$scaling[[1]])

      if (nrow(rst$scaling) == length(pos))
        rst$scaling[[1]] <- factor(rst$scaling[[1]],
                                   names(include.labels[pos]),
                                   as.character(include.labels[pos]))
      else{ stop("\nDie Labels mussen als c(a = 'Hallo', b = 'Welt') uebergeben werden.\n")}

      return(rst)
    }

  }
  }


#' @rdname extract
#' @export
tbll_extract.lda <-
  function(x,
           newdata = NULL,

           include.means = FALSE,
           include.scal = TRUE,
           include.cTab = TRUE,
           include.svd = TRUE,
            digits = 2,
           ...) {

    if(is.null(newdata)){
       newdata = model.frame(x)
       note_test <- note_train <- ""

       }
    else{
      note_test <- paste("Test-Data n = ", nrow( newdata ))
      note_train <- paste("Train-Data n = ", nrow( model.frame(x) ))
      }

    fit_predict <- MASS:::predict.lda(x, newdata)
    rslt <- list()

    if (include.means)
      rslt$means <-
        fix_and_format(t(x$means), caption = "Means", note = note_train, digits = digits)

    if (include.scal){
      rslt$scaling <-
        prepare_output(stp25tools::fix_to_df(
          render_f_signif(x$scaling, digits)),
          caption = "Coefficients of linear discriminants",
          note = note_train)

        est <- rslt$scaling[[2]]
        prm  <- rslt$scaling[[1]]
        est <- ifelse(grepl("\\-", est), gsub("\\-", "\\- ", est), paste("+", est))
        rst <- paste( "Index =", paste(paste(est, "x" , prm, sep = ""), collapse =                              " "))
        rslt$scaling.formula <- gsub("= \\+" , "=", rst)

      }

    if (include.svd) {
      svd <- x$svd
      names(svd) <- dimnames(x$scaling)[[2L]]
      if (length(svd) > 1L) {
        svd <- prepare_output(fix_format(data.frame(t(
          data.frame(svd ^ 2 / sum(svd ^ 2))
        ))), caption = "Proportion of trace",
        note = note_train)
      }
      rslt$svd <- svd
    }

    if (include.cTab) {
      xx <- newdata[[x$terms[[2L]]]]
      yy <- fit_predict$class
      cTab  <- table(xx, yy, dnn = c(x$terms[[2L]] , "Predict"))
      if (length(cTab) == 4L) {
        cat(" in 2x2 ")
        cTab <-  Klassifikation(cTab)
        rslt$cTab <-
          fix_and_format(
            cTab[[1]],
            caption = "Contingency table of actual and predicted categories",
            note = note_test,
            digits = 0)
        rslt$Accuracy <- cTab[[2]][c(1, 7, 8), ]
      }
      else {
        rslt$cTab <-
          fix_and_format(cTab,
                         caption = "Contingency table of actual and predicted categories",
                         note =note,
                         digits = 0)
        Accuracy <- NULL
        for (i in seq_len(dim(cTab)[1] - 1)) {
          msr <- colnames(cTab)[i + 1]
          cTab1 <-
            table(
            factor(xx == msr, c(TRUE, FALSE)) ,
            factor(yy == msr, c(TRUE, FALSE))
            )
          accr <-  Klassifikation(cTab1)[[2]][c(1, 7, 8), ]
          if (is.null(Accuracy))
            Accuracy <-  accr
          else
            Accuracy <- cbind(Accuracy, accr[2])
          names(Accuracy)[i + 1] <- msr
        }
        attr(Accuracy, "note") <- note_test
        rslt$Accuracy <- Accuracy
      }
    }
    rslt$predict <-
      list(data = cbind(
        newdata,
        fit_predict$posterior,
        predict = fit_predict$class,
        fit_predict$x))
    rslt
  }




fix_and_format <- function(x, caption, note = "", digits = 2, ...) {
  prepare_output(
    fix_format(
      stp25tools::fix_to_df(x),
      digits = digits,
      include.rownames = FALSE
    ),
    caption = caption,
    note = note
  )
}


