#' model_info
#'
#' Extrahiert die Variablen namen die Labels und die Daten
#'
#'
#'
#' @name model_info
#'
#' @param x Fit Objekt
#' @return Liste mit c("class",  "family", "y", "x", "labels", "N")
#' @export
#' @examples
#' model_info(mpg ~ cyl)
#' model_info(glm(vs ~ mpg, mtcars, family = binomial()))
#' model_info(lm(mpg ~ drat + wt + qsec, mtcars))
#' model_info(wilcox.test(mpg ~ vs, mtcars))
#' model_info(t.test(mpg ~ vs, mtcars))
#'
model_info <- function(x) {


  # Alternative ist die Library insight
  # mit zb der Funktion
  # model_info(m4<-Hmisc::spearman2(mpg ~ factor(vs), mtcars))
  # insight::model_info(m2)
  # insight::find_response(m4)
  # insight::find_predictors(m4)
  # aber es gehen noch nicht so viel Modelle

  UseMethod("model_info")
}

#' @rdname model_info
#' @export
model_info.default <- function(x) {
  list(
    class = class(x) ,
    family = NULL ,
    y = NULL ,
    x = NULL ,
    labels = NULL,
    N = NULL,
    missing = NULL
  )
}


#' @rdname model_info
#' @export
model_info.data.frame <- function(x) {
  list(
    class = class(x) ,
    family = NULL ,
    y = NULL ,
    x = NULL ,
    labels = NULL,
    N = nrow(x),
    missing = NULL
  )
}

#' @rdname model_info
#' @export
Infomation <- function(...) {
  my_fit <- list(...)
  names <- abbreviate(gsub("[~??+\\:=]", "",
                           as.character(as.list(sys.call(
                           )))[seq_len(length(my_fit)) + 1]),
                      minlength = 7)
  rslt <-
    data.frame(Info = c("Dependent Variable", "Observations", "Missing", "Type"))
  for (i in seq_along(my_fit)) {
    rs <-  model_info(my_fit[[i]])

    rslt[names[i]] <- c(rs$y, rs$N, rs$missing, rs$family[1])
  }

  rslt
}

#' @rdname model_info
#' @export

model_info.formula <- function(x) {
  fm <-  Formula_Names(x)
  list(
    class = class(x) ,
    family = NULL ,
    y = fm$yname,
    x = fm$xname,
    labels = NULL,
    N = NULL,
    missing = NULL
  )
}

#' @rdname model_info
#' @export

model_info.eff <- function(x) {
  list(
    #formula=fm,
    class = class(x)[1],
    family =  x$family[1:2]  ,
    y = x$response,
    x = names(x$variables) ,
    # data = x$model,
    labels =  stp25tools::get_label(x$data),
    N = nrow(x$data),
    missing = NULL
  )


}

#' @rdname model_info
#' @export
#' @examples
#' # model_info(Hmisc::spearman2(mpg ~ factor(vs), mtcars))
#'
model_info.htest <- function(x) {

  if (names(x$statistic) == "t") {
    list(
      class = "t.test" ,
      family = x$methode ,
      y = strsplit(x$data.name, " by ")[[1]][1] ,
      x = strsplit(x$data.name, " by ")[[1]][2] ,
      labels = NULL,
      N = NULL,
      missing = NULL
    )
  } else {
    list(
      class = "wilcox.test" ,
      family = x$methode ,
      y = strsplit(x$data.name, " by ")[[1]][1] ,
      x = strsplit(x$data.name, " by ")[[1]][2] ,
      labels = NULL,
      N = NULL,
      missing = NULL
    )
  }

}

#' @rdname model_info
#' @export
model_info.lm  <- function(x)
  model_info_glm(x)

#' @rdname model_info
#' @export
model_info.glm  <- function(x)
  model_info_glm(x)

#' @rdname model_info
#' @export
model_info.anova  <- function(x) {
  fm <- formula(x)
  y <- gsub("Response: ", "", attr(x, "heading")[2])
  x <- rownames(x)
  x <-  x[-length(x)]
  if (length(grep("\\:", x)) > 0)
    x <- x[-grep("\\:", x)]
  labels <- c(y, x)
  names(labels) <- labels
  list(
    class = "anova",
    family = c("anova", ""),
    y = y,
    x = x,
    labels =  labels,
    N = NA,
    missing = NULL
  )


}

# helper lm glm
model_info_glm <- function(x) {
  fm <- formula(x)
  list(
    class = class(x)[1],
    family = unlist(family(x)[1:2]) ,
    y = all.vars(formula(fm))[1],
    x = all.vars(formula(fm))[-1],
    labels =  stp25tools::get_label(x$model),
    N = nrow(x$model),
    missing = length(attr(x$model, "na.action"))

  )
}




#' @rdname model_info
#' @export
model_info.lmerModLmerTest <- function(x)
  model_info_lmer(x)

#' @rdname model_info
#' @export
model_info.merModLmerTest <- function(x)
  model_info_lmer(x)

#' @rdname model_info
#' @export
model_info.lmerTest <- function(x)
  model_info_lmer(x)

#' @rdname model_info
#' @export
model_info.glmerMod <- function(x)
  model_info_lmer(x)
#--   "merModLmerTest"  "lmerTest" "glmerMod"
model_info_lmer <- function(x) {
  fm <- formula(x)

  list(
    class = class(x)[1],
    family = unlist(family(x)[1:2]) ,
    y = all.vars(formula(fm))[1],
    x = all.vars(formula(fm))[-1],
    # data = x@frame,
    labels =  stp25tools::get_label(x@frame),
    N = nrow(x@frame),
    missing = NULL
  )

}

#' @rdname model_info
#' @export
model_info.lme <- function(x) {
    fm <-  insight::model_info(x)

  xn <- fm$model_terms$conditional
  yn <-   fm$model_terms$response

  list(
    class = class(x) ,
    family = fm$family ,
    y = yn,
    x =  xn,
    labels = stp25tools::get_label(insight::get_data(x)[-1]),
    N = fm$n_obs,
    missing = NULL
  )

}



#' @rdname model_info
#' @export
#' @examples
#' # model_info(coin::wilcox_test(mpg ~ factor(vs), mtcars))
#'
model_info.ScalarIndependenceTest <- function(x) {
  list(
    class = "coin" ,
    family = NULL ,
    y = NULL ,
    x = NULL ,
    labels = NULL,
    N = NULL,
    missing = NULL
  )
}

#' @rdname model_info
#' @export
#' @examples
#' # model_info(Hmisc::spearman2(mpg ~ factor(vs), mtcars))
#'
model_info.biVar <- function(x) {
  list(
    class = "biVar" ,
    family = NULL ,
    y = NULL ,
    x = NULL ,
    labels = NULL,
    N = NULL,
    missing = NULL
  )
}



#' @rdname model_info
#' @export
model_info.survfit <- function(x)
  model_info_surv(x)


#' @rdname model_info
#' @export
model_info.coxph <- function(x)
  model_info_surv(x)


model_info_surv <- function(x) {
  fm <- formula(x)
  list(
    class = class(x)[1],
    family = NULL,
    y = all.vars(formula(fm))[1],
    x = all.vars(formula(fm))[-1],
    labels = NULL,
    N = x$n,
    missing = NULL
  )
}

#' @rdname model_info
#' @export
model_info.survdiff <- function(x) {
  fm <- formula(x)
  list(
    class = class(x)[1],
    family = NULL,
    y = all.vars(formula(fm))[1],
    x = all.vars(formula(fm))[-1],
    labels = NULL,
    N = sum(x$n),
    missing = NULL
  )
}



# model_info.ICC <- function(x) {
#   ### # geht irgendwie noch nicht
#   list(
#     class =  class(x)[1] ,
#     family = NULL,
#     y = NULL,
#     x = NULL,
#     labels = NULL,
#     N = paste0("obs=", x$n.obs, ", judge=", x$n.judge)
#   )
# }


#' @rdname model_info
#' @export
model_info.polr <- function(x) {
  list(
    class = class(x)[1],
    family = x$method ,
    y = names(x$model)[1],
    x = names(x$model)[-1],
    labels =  stp25tools::get_label(x$model),
    N = x$n,
    missing = NULL
  )
}
