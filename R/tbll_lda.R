#' Tbll_lda
#'
#' Linear Discriminant Analysis
#'
#' @export
#' @param include.label character. Beschriftung auf Labels umstellen
#' @param ...  tbll_extract.lda:
#'  x  MASS::lda
#'  newdata  Test daten wenn Null dann wird model.frame(x)
#'  verwendet und an MASS::predict.lda weitergegeben.
#'  include.svd Eigenvalue
#'  include.cTab Accuracy Table
#'  include.scal scaling (LD)
#'  include.means aritmetic means
#'  digits digits
#'
#'
#' @return list with means, scal, cTab, include.svd and predictiv-Data
#'
#' @examples
#'
#' # Linear Discriminant Analysis
#' require(MASS)
#' #require(stp25stat2)
#' #require(stp25output2)
#' #require(stp25plot)
#' #require(lattice)
#'
#'
#' Iris <- data.frame(id = seq_len(50*3),
#'                    rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
#'                    Sp =  factor(rep(c("s","c","v"), rep(50,3))),
#'                    #add a bit of noise to the data
#'                    Species = factor(rep(
#'                    c("treat","treat",
#'                    "control", "control",
#'                    "treat", "control"),
#'                    rep(25,6)))
#' )
#'
#' # sample 70% of the observations
#' set.seed(11, sample.kind="Rejection")
#' train_data <-  Iris |> dplyr::slice_sample(prop=.70)
#' test_data <-   Iris |> dplyr::anti_join(train_data, by = 'id')
#'
#' # 1)  Logistic regression
#' # H0: b = 0
#'
#' # reg_log <-
#' #   glm(Species ~ Sepal.L. + Sepal.W. + Petal.L. + Petal.W.,
#' #       train_data,
#' #       family = "binomial")
#' # reg_lm <-
#' #   lm(I(Species == "s") ~ Sepal.L. + Sepal.W. + Petal.L. + Petal.W.,
#' #      train_data)
#' # reg_log$coefficients
#' # exp(reg_log$coefficients)
#' # coef(reg_lm)
#'
#' # 2) Linear and quadratic discriminant analysis
#'
#' ldamod <- lda(Species ~ Sepal.L. + Sepal.W. + Petal.L. + Petal.W.,
#' train_data)
#' ldamod$scaling
#'
#' lda_pred <- predict(ldamod, newdata = test_data)
#' # posterior probabilities
#' head(lda_pred$posterior)
#'
#' x_tab <- table(lda_pred$class, test_data$Species)
#' caret::confusionMatrix(x_tab)
#'
#' rslt <- Tbll_lda(ldamod,  test_data)
#' # rslt  |> Output()
#'
#'
#' #bwplot(LD1 ~ Species, rslt$predict[[1]],
#' # main ="Prediction of the test model")
#'
#' # 3) Construction and plotting of the ROC curve
#' # require(pROC)
#' #
#' # roc_lda <-  roc(response = test_data$Species,
#' #               predictor = lda_pred$posterior[,2])
#' # pROC::coords(roc_lda, "best")
#' # Tbll_roc(roc_lda)
#' # plot(roc_lda)
#' # plotROC2(roc_lda, include.table = TRUE)
#'
#'
Tbll_lda <- function(..., include.label = NULL){

  is_logical <-  NULL

  if(is.null(include.label)){
     return(tbll_extract.lda(...))
    }
  else {
    if (is_logical(include.label)) stop("\nHier musst du mir die Labels als Character-String uebergeben.\n")
    else{
      rst<- tbll_extract.lda(...)
      pos <- which(names(include.label)  %in% rst$scaling[[1]])

      if (nrow(rst$scaling) == length(pos))
        rst$scaling[[1]] <- factor(rst$scaling[[1]],
                                   names(include.label[pos]),
                                   as.character(include.label[pos]))
      else{ stop("\nDie Labels mussen als c(a = 'Hallo', b = 'Welt') uebergeben werden.\n")}

      return(rst)
    }

  }
  }


#' @rdname extract
#'

#' @param newdata lda. MASS::lda Test daten wenn Null dann wird model.frame(x)
#'  verwendet und an MASS::predict.lda weitergegeben.
#' @param include.svd lda. Eigenvalue
#' @param include.cTab lda. Accuracy Table
#' @param include.scal lda. scaling (LD)
#' @param include.means lda. aritmetic means
#'
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
    note <- ""
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
        fix_and_format(t(x$means),
                       caption = "Means",
                       note = note_train, digits = digits)

    if (include.scal){


     center_mean <-  mean(
       rowSums(mapply(function(x,t) x*t,
                      newdata[rownames(x$scaling)] ,
                      as.vector(x$scaling))))

      rslt$scaling <-
        prepare_output(stp25tools::fix_to_df(
          render_f_signif(x$scaling, digits)),
          caption = "Coefficients of linear discriminants",
          note = note_train)

        est <- rslt$scaling[[2]]
        prm  <- rslt$scaling[[1]]

        est <- ifelse(grepl("\\-", est), gsub("\\-", "\\- ", est), paste("+", est))
        rst <- paste( "Index =",
                      paste(paste(est, "x" , prm, sep = ""),
                            collapse = " "),
                      " - ", round(center_mean,2)
                      )
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
       # cat(" in 2x2 ")
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
      list(test_data = cbind(
        newdata,
        fit_predict$posterior,
        predict = fit_predict$class,
        fit_predict$x)
        )
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
# render_f_signif<- stp25stat2:::render_f_signif
# rslt <- Tbll_lda(ldamod,  test_data)
#
# rslt$scaling.formula
