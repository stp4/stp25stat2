#' Kreuztabellen
#'
#'
#' @export
#'
#' @examples
#'
#'
#'  data(infert, package = "datasets")
#' infert$case  <- factor(infert$case , 1:0, c("case", "control"))
#'
#' infert$spontaneous <- factor(infert$spontaneous)
#' infert$induced2    <- factor(infert$induced == 0)
#'
#' Tbll_xtabs( ~  case, infert)
#' Tbll_xtabs( ~ induced2 + case, infert)
#' Tbll_xtabs( ~ induced + case, infert)
#' Tbll_xtabs( ~ induced + education, infert)
#'
#'
#' Tbll_xtabs( ~ induced + education + case,
#'             infert,
#'             margin = "case",
#'             #  add.margins = c("education", "induced"),
#'             include.count = FALSE)
#'
#' Tbll_xtabs(
#'   ~ induced + education + case,
#'   infert,
#'   margin = "case",
#'   add.margins = c("case"),
#'   include.count = FALSE
#' )
#'
#'
#'
Tbll_xtabs <-   function(x, ...) {
  UseMethod("Tbll_xtabs")
}

#' @rdname Tbll_xtabs
#'
#' @export
#'
Tbll_xtabs.default <- function(x, ...) {
  cat("Keine Methode fuer ", class(x), " vorhanden.")
}

#' @rdname Tbll_xtabs
#'
#' @export
Tbll_xtabs.NULL <- function() {
  Info_Statistic(
    c(
      "include.chisq",
      "include.fisher",
      "include.correlation",
      "include.diagnostic"
    ),
    c("vcd", "stats", "vcd", "caret"),
    c(
      "assocstats",
      "fisher.test",
      "assocstats",
      "confusionMatrix"
    ),
    paste(methods("Tbll_xtabs"), collapse = ", ")
  )
}

#' @rdname Tbll_xtabs
#'
#' @export
Tbll_xtabs.glm <- function(x,
                          thresh = 0.5,
                          ...) {

 Klassifikation(x, thresh, caption)$xtab

}

#' @rdname Tbll_xtabs
#'
#' @export
#' @param x,data formula und data.frame geht an xtabs
#' @param addNA,exclude,drop.unused.levels an xtabs()
#' @param margin,add.margins Prozent und total
#' @param ... include.test usw
#' @examples
#'
#' df<- data.frame(A = c(1,0,0,1,0,1,0,1,1,0,0,0,0,1,1),
#' B = c(0,0,1,0,1,0,1,1,1,1,1,1,1,0,0)
#' )
#'
#'
#' Tbll_xtabs(
#'   ~ A + B,
#'   df,
#'   include.percent = FALSE,
#'   include.test = TRUE,
#'   include.diagnostic = TRUE
#' )
#'
Tbll_xtabs.formula <-
  function(x,
           data = NULL,
           addNA = FALSE,
           exclude = if (!addNA) c(NA, NaN),
           drop.unused.levels = FALSE,
           margin = NULL,
           add.margins = NULL,
           ...) {
    x_tab <- stats::xtabs(
      x, data,
      addNA = addNA,
      exclude = exclude,
      drop.unused.levels = drop.unused.levels
    )

    dnn <- dimnames(x_tab)
    names(dnn) <- stp25tools:::get_label2(data[all.vars(x)] )
    dimnames(x_tab) <- dnn

    if (is.character(margin))
      margin <- which(all.vars(x) == margin)
    if (is.character(add.margins))
      add.margins <- which(all.vars(x) == add.margins)

    #  if (is.logical(labels)) {
    #    if (labels) {
    # dnn <- dimnames(x_tab)
    # names(dnn) <-
    #   stp25tools::get_label(data[all.vars(x)], include.units = FALSE)
    # dimnames(x_tab) <- dnn
    #  }
    #  } else if (is.character(labels)) {
    #   dnn <- dimnames(x_tab)
    #   names(dnn)[1:length(labels)] <- labels
    #   dimnames(x_tab) <- dnn
    # } else if (is.list(labels)) {
    #   dimnames(x_tab) <- labels
    # }

    Tbll_xtabs.xtabs(x_tab,
                     margin = margin,
                     add.margins = add.margins,
                     ...)
  }






#' @rdname Tbll_xtabs
#'
#' @export
#'
Tbll_xtabs.data.frame <-
  function(data ,
           ...,
           include.count = TRUE,
           include.percent = TRUE,
           include.prop.chisq = FALSE,
           include.chisq = FALSE,
           include.fisher = FALSE,
           include.test = any(c(include.fisher, include.chisq, include.prop.chisq)),
           include.correlation = FALSE,
           include.diagnostic = FALSE,
           margin = NULL,
           add.margins = NA,
           digits = get_opt("prozent", "digits"),
           prevalence = NULL,
           addNA = FALSE,
           exclude = if (!addNA) c(NA, NaN),
           drop.unused.levels = FALSE) {

    X <- stp25tools::prepare_data2(data, ...)
if(!is.null( X$group.vars ))
  stop("group.vars sind fuer xtabs nicht definert!")

    Tbll_xtabs.formula(
      as.formula(paste(
        "~", paste(X$measure.vars, collapse = "+"))),
      X$data,
      include.count = include.count,
      include.percent = include.percent,
      include.prop.chisq = include.prop.chisq,
      include.chisq = include.chisq,
      include.fisher = include.fisher,
      include.test = include.test,
      include.correlation = include.correlation,
      include.diagnostic = include.diagnostic,
      prevalence = prevalence,
      margin = margin,
      add.margins = add.margins,
      digits = digits,
      addNA = addNA,
      exclude = exclude,
      drop.unused.levels = drop.unused.levels
    )
  }

#' @rdname Tbll_xtabs
#'
#' @export
#'
Tbll_xtabs.table <- function(...) Tbll_xtabs.xtabs(...)


#' @rdname Tbll_xtabs
#'
#' @export
#'
#' @param digits Nachkommastellen
#' @param include.percent,include.count ausgabe
#' @param include.test,test,include.prop.chisq,include.chisq,include.fisher die Tests
#' @param include.correlation Korrelation
#' @param include.diagnostic,include.sensitivity,prevalence ascostat
#' @param ... not used
#'
#' @return list("xtab","fisher_test","diagnostic.test")
#'
#' @examples
#'
#'
#'
#' data(infert, package = "datasets")
#' infert$case  <- factor(infert$case ,1:0, c("case", "control") )
#'
#' infert$spontaneous <- factor(infert$spontaneous)
#' infert$induced2    <- factor(infert$induced==0)
#'
#' tab_1<- xtabs(~  case, infert)
#' tab_2x2<- xtabs(~ induced2 + case, infert)
#' tab_3x2<- xtabs(~ induced + case, infert)
#' tab_3x3<- xtabs(~ induced + education, infert)
#' tab_3x3x2<- xtabs(~ induced + education+case, infert)
#'
#' #Tbll_xtabs(summary(tab_3x3x2))
#'
#' (Tbll_xtabs(tab_1, include.test=TRUE))
#' (Tbll_xtabs(tab_2x2, include.test=TRUE))
#' (Tbll_xtabs(tab_3x2, include.test=TRUE))
#' (Tbll_xtabs(tab_3x3, include.test=TRUE))
#' (Tbll_xtabs(tab_3x3x2, include.test=TRUE))
#'
Tbll_xtabs.xtabs  <- function(x,
                              include.count = TRUE,
                              include.percent = TRUE,
                              include.prop.chisq = FALSE,
                              include.chisq = FALSE,
                              include.fisher = FALSE,
                              include.test = any(c(include.fisher, include.chisq, include.prop.chisq)),
                              include.correlation = FALSE,
                              include.diagnostic = FALSE,
                              margin = NULL,
                              add.margins = NULL,
                              digits = get_opt("prozent", "digits"),
                              prevalence = NULL,
                              ...) {
  # cat( "\n in Tbll_xtabs.xtabs\n" )
  res <- list()
  dim_x <- dimension(x)
  # get position of margin
  var_nms <- names(dimnames(x))
  if (is.character(margin))
    margin <- which(var_nms == margin)
  if (is.character(add.margins))
    add.margins <- which(var_nms == add.margins)

  res$xtab <- prepare_output(
    format_xtab(
      x,
      margin = margin,
      # mrgn$prop,
      add.margins =  add.margins,
      #mrgn$add,
      include.count,
      include.percent,
      digits = digits,
      dim_x = dim_x
    ),
    caption = "Haeufigkeitstabellen"
  )

  if (include.test) {
    include.chisq.sumary <- FALSE
    if (!any(include.fisher, include.chisq, include.prop.chisq)) {
      dm <- dim(x)
      ldm <-  length(dm)
      if (ldm == 1)
        include.prop.chisq <- TRUE
      else if (ldm == 2 &
               prod((dm - 1)) == 1)
        include.fisher <- TRUE
      else if (ldm == 2)
        include.chisq <- TRUE
      else
        include.chisq.sumary <- TRUE
    }
    if (include.prop.chisq) {
      cat(
        "\nFunktion  Proportion noch nicht fertig. Daher bitte APA(binom.test(tab_1)) verwenden.\n"
      )
      res$prop.chisq <- NULL
    }
    else if (include.fisher & dim_x == 1) {
      fisher_test <- fisher.test(x)
      res$fisher_test <- prepare_output(
        data.frame(
          OR  = render_f(fisher_test$estimate),
          CI  = rndr_CI(matrix(fisher_test$conf.int, ncol = 2)),
          p   = rndr_P(fisher_test$p.value),
          stringsAsFactors = FALSE
        ),
        caption = "Fisher's Exact Test"
      )
    }
    else if (include.chisq & dim_x == 2) {
      chisq_tests <-  vcd::assocstats(x)
      res$chisq_tests <- prepare_output(
        data.frame(
          Test = rownames(chisq_tests$chisq_tests),
          Chi2 = render_f(chisq_tests$chisq_tests[, 1], 2),
          df   = render_f(chisq_tests$chisq_tests[, 2], 0),
          p    = rndr_P(chisq_tests$chisq_tests[, 3]),
          stringsAsFactors = FALSE
        ),
        caption = "Chi-Squared Test"
      )
    }
    else if (include.chisq.sumary) {
      # hier gibt es noch eine spezifikation
      res$chisq_tests <- Tbll.summary.table(summary(x))
    }
    else {
      res$chisq_tests <- Tbll.summary.table(summary(x))
    }
  }

  if (include.correlation) {
    corr_test <-  vcd::assocstats(x)
    res$corr_test <- prepare_output(data.frame(
      Test = c("Phi-Coefficient",
               "Contingency Coefficient",
               "Cramer's V"),
      r = render_f(
        c(corr_test$phi,
          corr_test$contingency,
          corr_test$cramer),
        3
      ),
      stringsAsFactors = FALSE
    ),
    caption = "Correlation Test")
  }

  if (include.diagnostic) {
    if(dim_x == 1)
    res$diagnostic.test <-
      prepare_output(
        Klassifikation(x,
                       prevalence = prevalence)$statistic,
                     caption = "Diagnostic")
    else {
      warning("\nDie Diagnostic gibt es nur bei 2x2-Tabellen (wir haben hier die Dimensions von ", dim_x, ").\n")
      }
  }

  res
}






# @rdname Tbll_xtabs
# @param ... MASS::loglm()
# @param names Quelle
# @export
#
# @examples
#
# 1+1

## example from Venables and Ripley (2002, pp. 190-2.)
# ldose <- rep(0:5, 2)
# numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
# sex <- factor(rep(c("M", "F"), c(6, 6)))
# SF <- cbind(numdead, numalive = 20 - numdead)
# budworm.lg <- glm(SF ~ sex * ldose, family = binomial)
# summary(budworm.lg)
#
#
# Tbll_reg_long(
#   budworm.lg,
#   include.se = TRUE,
#   include.ci = FALSE,
#   include.p = TRUE
# )
#
#
# dat <-
#   stp25aggregate::GetData(
#     "alter  baum ohne mit
# 1 buche 20  2
# 1 fichte 11 1
# 1 larch 7 1
# 2 buche 18 13
# 2 fichte 3 6
# 2 larch 4 7
# 3 buche 8 21
# 3 fichte 1 1
# 3 larch 1 5
# "
#   )
#
# dat <-  stp25aggregate::Long(dat,
#                              ohne,
#                              mit,
#                              by = ~ alter + baum,
#                              key = "defol",
#                              value = "Freq")
# dat <-
#   dplyr::mutate(dat, alter = factor(alter, 1:3, c("<70", "71-140", "140")))
#
# xt <- xtabs(Freq ~ alter + baum + defol, dat)
#
#
#
#
# Tbll(summary(xt))
# fit_xt <- glm(Freq ~ alter + baum + defol, dat, family = poisson())
# #lmtest::lrtest(fit_xt)
# MASS::loglm( ~ alter + baum + defol, data = xt)
# MASS::loglm(fit_xt)
#
# Tbll_xtabs(xt, include.percent = FALSE)
#
# x <-  MASS::loglm(~ alter * baum * defol  , data = xt)
# Tbll(x)
#
# #Log-Linear Models
# Log_Linear_Models <- function(..., names) {
#   rslt <- NULL
#   models <- list(...)
#
#   for (i in seq_along(models)) {
#     rslt <- rbind(rslt,
#                   cbind(
#                     formula = paste(formula(models[[i]]), collapse = " "),
#                     Model = names[i],
#                     Tbll(models[[i]])[1, -1]
#                   ))
#   }
#   rslt
#
# }
# Log_Linear_Models(
#   MASS::loglm(~ alter * baum * defol  , data = xt),
#   MASS::loglm(~ (alter + defol) * baum  , data = xt),
#   MASS::loglm(~ alter * baum + baum * defol, data = xt),
#   MASS::loglm(~ alter * baum + defol, data = xt),
#   MASS::loglm(~ alter + baum + defol, data = xt),
#
#   names = c(
#     "satturiert [ABC]",
#     "[AB][BC]",
#     "[AB][BC]",
#     "[AB][C]",
#     "[A][B][C]"
#   )
# )
#

# Tbll_loglm <- function(..., names = NULL ) {
#   models<- list(...)
#   if(is.null(names)) paste("M", seq_along(models))
#   rslt<- NULL
#
#   for(i in seq_along(models)){
#
#     rsl <- tbll_extract(models[[i]], include.pearson=FALSE)
#     rsl[1, 1] <- names[i]
#     rslt <- rbind( rslt, rsl)
#   }
#   rslt
# }





# Helpers -----------------------------------------------------------------


#' @rdname Tbll_xtabs
#' @examples
#'
#' a <- letters[1:3]
#' Tbll(summary(table(a, sample(a))))
#'
Tbll.summary.table <- function(x, ...) {
  prepare_output(data.frame(
    Chisq =    render_f(x$statistic, 2),
    df = x$parameter,
    p =  rndr_P(x$p.value, FALSE)
  ),
  caption = "Pearson's Chi-squared Test for Count Data")
}

#' main function for xtabs
#'
#' @noRd
#' @param x xtabs Tabelle
#' @param margin,add.margins welche Margins
#' @param include.count,include.percent Include
#' @param digits Komastellen
#' @param dim_x was fuer eine Tabelle kommt
format_xtab <- function(x,
                        margin = NULL,
                        add.margins = NULL,
                        include.count = TRUE,
                        include.percent = TRUE,
                        digits =  get_opt("prozent", "digits"),
                        dim_x = dimension(x),
                        style = get_opt("prozent", "style"))  {
  style <-
    if (include.count & include.percent) style
    else if (include.count &  !include.percent) 4
    else if (!include.count &  include.percent) 3
    else 1


  if (dim_x > 0) {
    if (!is.null(add.margins)) {
    #   cat("\n format_xtab \n")
    #   cat("  add.margins:  ")
    #   print(add.margins)
    # #  print(addmargins(x, add.margins))
    #   cat("\n-----------------\n")
      cnt <- ftable(addmargins(x, add.margins))

      prop_table <- prop.table(x, margin)
      prop_table[which(is.na(prop_table))] <- 0
      prc <-
        ftable(addmargins(prop_table * 100,
                          add.margins))
    } else{
      cnt <- ftable(x)
      prop_table <- prop.table(x, margin)
      prop_table[which(is.na(prop_table))] <- 0
      prc <-  ftable(prop_table * 100)
    }

    return(rndr_percent(prc, cnt, digits = digits, style = style))

  }
  else{
    # 1 x Tabelle
    prc <- prop.table(x) * 100
    rslt <-
      stp25tools::fix_to_df(rndr_percent(prc, x, digits = digits, style = style))
    names(rslt)[1:2] <- c(names(dimnames(x)), "m")

    return (rslt)
  }
}


dimension <- function(x) {
  dm <- dim(x)
  ldm <-  length(dm)
  if (ldm == 1)  0
  else if (ldm == 2 & prod((dm - 1)) == 1)  1
  else if (ldm == 2)  2
  else ldm
}





# Klassifikation ----------------------------------------------------------


#' @rdname Tbll_xtabs
#'
#' @description Classification Table  classification_table
#'  Richtige und falsche Klassifikationen
#'  Bei  2x2 Tabellen der Kappa Test
#'
#' Sensitivity = A/(A+C)
#'
#' Specificity = D/(B+D)
#'
#' Prevalence = (A+C)/(A+B+C+D)
#'
#' PPV = (sensitivity * prevalence)/((sensitivity*prevalence) + ((1-specificity)*(1-prevalence)))
#'
#' NPV = (specificity * (1-prevalence))/(((1-sensitivity)*prevalence) + ((specificity)*(1-prevalence)))
#'
#' Detection Rate = A/(A+B+C+D)
#'
#' Detection Prevalence = (A+B)/(A+B+C+D)
#'
#' Balanced Accuracy = (sensitivity+specificity)/2
#'
#' Precision = A/(A+B)
#'
#' Recall = A/(A+C)
#'
#' F1 = (1+beta^2)*precision*recall/((beta^2 * precision)+recall)
#'
#'
#' Klassifikation fuer Binominal-GLM
#'
#' @param x glm oder xtab Objekt
#' @param thresh Klassifikation auf Basis der Vorhersage Schwelle bei P=0.5
#' @param caption an Output
#' @param ... weitere Objekte nicht benutzt
#' @return A data.frame Objekt.
#' @export
#' @examples
#'
#'  require(stp25data)
#'
#' x<-xtabs(~gruppe+lai, hkarz)
#'
#' Klassifikation(x)
Klassifikation <- function(x, ...) {
  UseMethod("Klassifikation")
}




#' @rdname Tbll_xtabs
#' @description Klassifikation.glm
#' @export
#' @examples
#'
#' # require(stp25data)
#' # fit1<- glm(gruppe~lai, hkarz, family = binomial)
#' # thkarz <- as.data.frame(xtabs(~gruppe+lai, hkarz))
#' # fit2<- glm(Freq ~ gruppe*lai, thkarz, family = poisson())
#'
Klassifikation.glm <-
  function(x,
           thresh = 0.5,
           ...) {
    response <- all.vars(formula(formula(x)))[1]
    data <- x$model
    predictor <- fitted(x) # vorhergesagte Wahrscheinlichkeit
    data$Response <- data[, response]


    mylevels <-
      if (is.factor(data$Response)) levels(data$Response)
      else 0:1

    data$Predictor <- cut(predictor,
                          breaks = c(-Inf, thresh, Inf),
                          labels = mylevels)
#print(car::some(data))

    # Kontingenztafel: tatsaechliche vs. vorhergesagte Kategorie
    cTab <- stats::xtabs( ~ Response + Predictor, data = data)

    if (length(cTab) == 4) {
      res <- Klassifikation.xtabs(cTab)

      res$response = data$Response
      res$predictor = predictor
    }
    else
      res <- list(
        xtab = cTab,
        statistic = NULL,
        response = response,
        predictor = predictor
      )
    res
  }

#' @rdname Tbll_xtabs
#' @export
Klassifikation.table <- function(...) Klassifikation.xtabs(...)

#' @rdname Tbll_xtabs
#' @description xtabs-Objekt
#' @export
#' @examples
#'
#'  # require(stp25data)
#'  # hkarz$LAI<- factor(hkarz$lai, 0:1, c("pos", "neg"))
#'  # Klassifikation(xtabs(~gruppe+LAI, hkarz), test=TRUE, type="fischer")
#'
#'  tab <- matrix(c(94, 40, 39, 40), ncol = 2, byrow = TRUE)
#'  tbll_extract(caret::confusionMatrix(tab))
#'  tbll_extract(epiR::epi.tests(tab) )
#'
#'  Klassifikation(as.table(tab))
#'
#'
#' # dat <-
#' #   get_data("
#' # outcome    A B
#' # a   94 40
#' # b   39  40",
#' #            tabel_expand = TRUE,
#' #            value = "test")
#' #
#' #
#' # dat$outcome <- ifelse(dat$outcome =="a",1,0)
#' # dat$test <- ifelse(dat$test =="A",1,0)
#' # xtabs( ~ outcome + test, dat)
#' #
#' # fit<- glm(outcome~ test, dat, family = binomial())
#' #
#' #
#' # summary(fit)
#' # dat$residuals <- residuals(fit)
#' # dat$predict <- predict(fit)
#' #
#' # (dat)
#' #
#' # require(glue)
#' # require(pROC)
#' # fit_roc <- pROC::roc( dat$outcome, dat$test )
#' # Tbll( fit_roc )
#' # ?coords(fit_roc)
#' #
#' #
#' # roc.liver <- roc( outcome ~ predict, dat)
#' #
#' # auc<- roc.liver$auc
#' # glue( 'Area under the curve: {sprintf("%.3f", auc)} it means there is a {sprintf("%.0f", auc*100)} % chance that the model
#' #  will be able to distinguish between positive class and negative class.')
#' #
#' #
#' # plot(roc.liver)
#'
Klassifikation.xtabs <-
  function(x,
           lvs = c("positiv", "negativ"),
           digits = 2,
           prevalence = NULL,
           ...) {
    if (!length(x) == 4)
      stop("Klassifikation: nur mit 2x2 Tabellen moeglich!")

    Positive_Class <-
      paste(attr(x, "dimnames")[[1]][1],
            attr(x, "dimnames")[[2]][1], sep = "/")


    attr(x, "dimnames")[[1]] <- lvs
    attr(x, "dimnames")[[2]] <- lvs

    x_asco <- caret::confusionMatrix(x, prevalence = prevalence)


    list(
      xtab = x ,
      statistic = tbll_extract.confusionMatrix(x_asco,
                                               digits = digits,
                                               Positive_Class = Positive_Class)
    )


    # out <- as.character(
    #   c(
    #     render_f(x$overall["Accuracy"], digits),
    #     rndr_CI(x$overall[c("AccuracyLower", "AccuracyUpper")]),
    #     render_f(x$overall["AccuracyNull"], digits),
    #     rndr_P(x$overall["AccuracyPValue"]),
    #     render_f(x$overall["Kappa"], digits),
    #     rndr_P(x$overall["McnemarPValue"]),
    #     render_f(x$byClass, digits),
    #     Positive_Class
    #   )
    # )
    #
    #
    # list(
    #   xtab = xtab,
    #   statistic =
    #     data.frame(
    #       Statistic =
    #         c(
    #           "Accuracy",
    #           "95% CI",
    #           "No Information Rate",
    #           "P-Value [Acc > NIR]",
    #           "Kappa",
    #           "Mcnemar's Test P-Value",
    #           "Sensitivity",
    #           "Specificity",
    #           "Pos Pred Value" ,
    #           "Neg Pred Value",
    #           "Precision",
    #           "Recall",
    #           "F1",
    #           "Prevalence",
    #           "Detection Rate",
    #           "Detection Prevalence" ,
    #           "Balanced Accuracy",
    #           "Positive Class"
    #         )
    #       ,
    #       Value = out,
    #       stringsAsFactors = FALSE
    #     ),
    #   Positive_Class =Positive_Class
    #   #,
    # #  stat = x,
    # #  response = NULL,
    # #  predictor = NULL
    # )
  }


# beispiel ----------------------------------------------------------------
# require(stp25tools)
# prepare_output <- stp25stat2::prepare_output
# rndr_percent <- stp25stat2:::rndr_percent
# render_f <- stp25stat2:::render_f
# rndr_P <- stp25stat2:::rndr_P
# rndr_CI <- stp25stat2:::rndr_CI
#
# # #detach("package:stp25stat2", unload = TRUE)
# #
# data(infert, package = "datasets")
#
# infert2  <- infert
# infert2$case  <- factor(infert2$case ,1:0, c("case", "control") )
# infert2<- Label(infert2,
#                 education = "Education",
#                 induced = "Number of prior Induced abortions",
#                 case = "Case Status")
#
#
# x <- xtabs( ~ education + induced + case, infert2)
#
# Tbll_xtabs(
#   x,
#   margin = "case",
#   add.margins = c("induced"))
#
# Tbll_xtabs(~ education + induced + case, infert2,
#            include.test = TRUE,
#            include.correlation = FALSE,
#            include.diagnostic = TRUE,
#
#   margin = "case",
#   add.margins = c("induced"))



 # summary(infert2)
 # levels(infert2$education) <- c("0-11yrs", "0-11yrs", "12+ yrs")
 # Klassifikation(xtabs(~ case + education, infert2), test=TRUE, type="fischer")
 #
 #
 # xtabs(~ induced + case, infert2)
 #
 # contrasts =  c("contr.treatment", "contr.poly")
 # options(contrasts = contrasts)
 #  fit1<- glm(case ~  education, infert2, family = binomial)
 #
 #  summary(fit1)
 #
 #  Klassifikation(fit1)


 #' # thkarz <- as.data.frame(xtabs(~gruppe+lai, hkarz))
 #' # fit2<- glm(Freq ~ gruppe*lai, thkarz, family = poisson())
