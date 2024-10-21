#' Tabellen aus pROC::roc
#'
#' @param x pROC::roc Objekt
#' @param digits numeric.
#' @param include.order logical.
#' @param ...
#'
#' @return data.frame
#' @export
#'
#' @examples
#' # example code
#' #'
#' require(pROC)
#' n <- 200
#' set.seed(n)
#' D.ex <- rbinom(n, size = 1, prob = .5)
#' re_scale <- function(x, min=0, max=2){
#'   x <- x - min(x)
#'   x <- x/max(x)
#'   x <- x*(max - min)
#'   round(x + min, 2)
#' }
#' DF <- data.frame(
#'   Group = factor(c("Control", "Ill")[D.ex + 1]),
#'   Cys = re_scale(rnorm(n, mean = D.ex, sd = .65), 18,84),
#'   Glu = re_scale(exp(2 - (D.ex) - rnorm(n, mean = 0, sd = .5)), 23, 537),
#'   Arg = re_scale(rnorm(n, mean = D.ex, sd = 2), 41,216),
#'   Gln = re_scale(rnorm(n, mean = D.ex, sd = 1.5) + D.ex, 163,1129)
#' ) |> stp25tools::Label(
#'   Arg  = "Arginine",
#'   Cit = "Citrulline",
#'   Cys = "Cysteine",
#'   Glu = "Glutamate",
#'   Gln = "Glutamine"
#' )
#'
#'
#' roc1 <- pROC::roc(Group ~ Glu, data = DF)
#' Tbll_roc(roc1)
#' # plotROC2(roc1)
#'
#' roc.list <- pROC::roc(Group ~ Glu+Gln+Arg+Cys, data = DF)
#' Tbll_roc(roc.list)
#'
#' #plotROC2(roc.list)
Tbll_roc <-
  function(x,
           digits = 2,
           include.order = TRUE,
           ...) {
    if (inherits(x, "roc")) {
      prepare_output(extract.roc(x, digits = digits),
                     roc_dataline(x))
    }
    else{
      rslt <- NULL
      for (i in names(x)) {
        rslt <- rbind(rslt, extract.roc(x[[i]], digits = digits))
      }

      if (include.order)
        rslt <- rslt[order(rslt[[2]], decreasing = TRUE), ]

      prepare_output(rslt)
    }


  }




#' @noRd
roc_dataline<-
  function (x) {
    if ("cases" %in% names(x$call) && "controls" %in% names(x$call)) {
      cat("Data: ", length(x$controls), " controls ", x$direction,
          " ", length(x$cases), " cases.\n", sep = "")
    }
    else {
      if ("predictor.name" %in% names(x)) {
        predictor.name <- x$predictor.name
      }
      else if ("predictor" %in% names(x$call)) {
        predictor.name <- as.character(x$call[match("predictor",
                                                    names(x$call))])
      }
      else if (!is.null(x$call$formula)) {
        indx <- match(c("formula", "data", "weights", "subset",
                        "na.action"), names(x$call), nomatch = 0)
        temp <- x$call[c(1, indx)]
        temp[[1]] <- as.name("model.frame")
        m <- eval.parent(temp, n = 2)
        response.name <- names(m)[1]
        predictor.name <- names(m)[-1]
      }
      else {
        predictor.name <- "(unknown)"
      }
      if ("response.name" %in% names(x)) {
        response.name <- x$response.name
      }
      else if ("response" %in% names(x$call)) {
        response.name <- as.character(x$call[match("response",
                                                   names(x$call))])
      }
      else if (!is.null(x$call$formula)) {
      }
      else if ("x" %in% names(x$call)) {
        response.name <- as.character(x$call[match("x", names(x$call))])
      }
      else {
        response.name <- "(unknown)"
      }
      paste("Data: ", predictor.name, " in ", length(x$controls),
            " controls (", response.name, " ", x$levels[1], ") ",
            x$direction, " ", length(x$cases), " cases (", response.name,
            " ", x$levels[2], ").", sep = "")
    }
  }

#' Youden’s J statistic
#'
#' Optimal (Youden Index) point
#'
#' The Youden index uses the maximum vertical distance of the ROC curve from the point (X, Y)
#' on the diagonal (random line). In fact, the Youden index maximizes the difference
#' between the Se and FP rate, in other words, it maximizes the percentage of Net correct
#' classification:
#'
#' Therefore, the optimal cut-off point is calculated by maximizing Se+Sp at
#' different cut-off points
#'
#' Quelle: https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-024-02198-2
#'





extract.roc <-
  function(x, ..., digits = 1) {
    youde<- pROC::coords(x, "best")

    rslt <-  stp25stat2::fix_format(
      data.frame(
        AUC =  as.numeric(x$auc),
        YI_Specificity = youde[2],
        YI_Sensitivity = youde[3],
        YI_Cutoff = youde[1]
      ),
      digits = c(NA, 2, 2, 2, digits)
    )

    rslt[[1]] <-
      if (!is.null(attr(x$predictor, "label")))
        attr(x$predictor, "label")
    else
      x$predictor.name
    rslt

  }



