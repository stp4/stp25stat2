#' Extract SEM
#'
#' lavaan: Struckturgleichungs Modelle mit lavaan.
#'
#' @param x Objekt
#' @param baseline.model an lavaan
#' @param include.ci,include.model,include.varianz,include.latent Lavan: was soll ausgegeben werden
#' @param ...  Fehler abfangen
#'
#' @rdname Tbll_cfa
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' # require(psych)
#' require(lavaan)
#' # require(stp25stat2)
#' # require(stp25output2)
#'
#' #'   #  Loading ist dabei der standartisierte Estimate und Communality ist die quadrierte Ladung
#' #'   #  Backhaus Multivariate Analysemethoden 11 AuflageSeite 383
#' #'   #  Moosbrugger, Kelava 2012 Testtheorie 2. Auflage Seite 339
#' #'
#' #'  require(lavaan)
#' #'  # require(tidyverse)
#' #'
#' model <- '
#'   # latent variable definitions
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + a*y2 + b*y3 + c*y4
#'      dem65 =~ y5 + a*y6 + b*y7 + c*y8
#'
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'
#'   # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#' '
#'
#' #fit <- sem(model, data = PoliticalDemocracy)
#' #Tbll(fit, include.varianz=FALSE)
#'
#' model <- '
#'   # latent variable definitions
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 +  y2 +  y3 +  y4
#'      dem65 =~ y5 +  y6 +  y7 +  y8
#'
#' '
#' fit2  <- cfa(model, data = PoliticalDemocracy,
#'              se = "robust",
#'              estimator = "MLM",
#'              missing = "listwise")
#'
#' x<-Tbll(
#'   fit2,
#'   fit.measures = TRUE,
#'   standardized = TRUE,
#'   include.varianz = TRUE,
#'   include.ci = TRUE
#' )
#' summary(fit2)
#' x$latent # |> Output()
#'
#' x$varianz # |> Output()
#' x  # |> Output("Hallo")
#'
#'
#' library(piecewiseSEM)
#' model <- '
#'   rich ~ cover
#'   cover ~ firesev
#'   firesev ~ age
#' '
#' fit2  <- cfa(model, data = keeley,
#'              se = "robust",
#'              estimator = "MLM",
#'              missing = "listwise")
#'
#' summary(fit2)
#' mod <- psem(
#'   lm(rich ~ cover, data = keeley),
#'   lm(cover ~ firesev, data = keeley),
#'   lm(firesev ~ age, data = keeley),
#'   data = keeley
#' )
#'
#' summary(mod)
#'
#' #' https://jslefche.github.io/sem_book
#' #' https://www.seascapemodels.org/structural-causal-models-tutorial/scm-tute.html
#' #'
#' #' An Introduction to Structural Equation Modeling in R
#'
#' # library(ggplot2)
#' # #library(patchwork)
#' # library(ggdag)
#' #
#' # library(dagitty)
#' # theme_set(theme_dag())
#' #
#' # keeley_mod <- dagify(  rich ~ cover,
#' #                     cover ~ firesev,
#' #                      firesev ~ age
#' #                     )
#' #
#' # g1 <- ggdag(keeley_mod, text_size = 2,
#' #             node_size = 12)
#' #
#' # g1
#'
#'  }
#'
tbll_extract.lavaan <- function(x,
                        baseline.model = NULL,
                        include.ci = FALSE,
                        include.model = TRUE,
                        include.varianz = FALSE,
                        include.latent = TRUE,
                        ...) {
  rslt <- list()

  res_lavaan <- as.list(lavaan::fitMeasures(
    x,
    fit.measures = c(
      "chisq",
      "df",
      "pvalue",
      "baseline.chisq" ,
      "baseline.df",
      "baseline.pvalue" ,
      "rmsea" ,
      "srmr",
      "gfi",
      "agfi",
      "cfi" ,
      "nfi"
    ),
    baseline.model =  baseline.model
  ))


  res_lavaan$df <- res_lavaan$df + 1  # unbekannter Fehler

  res_lavaan <- with(
    res_lavaan,
    rbind(
      "ML"  = c(rndr_Chisq(chisq, df, pvalue), ""),
      "Model test baseline model" = c(
        rndr_Chisq(baseline.chisq, baseline.df, baseline.pvalue),
        ""
      ),
      "Number of observations" = c(x@Data@nobs[[1]], ""),
      "chisq/df" = c(render_f(chisq / df, 2), rndr_Chisq_cfa(chisq, df)),
      "GFI" = c(render_f(gfi, 2), rndr_gfi_cfa(gfi)),
      "AGFI" = c(render_f(agfi, 2), rndr_agfi_cfa(agfi)),
      "NFI"  = c(render_f(nfi, 2), rndr_nfi_cfa(nfi)),
      "CFI" =  c(render_f(cfi, 2), rndr_cfi_cfa(cfi)),
      "RMSEA" = c(render_f(rmsea, 2), rndr_rmsea_cfa(rmsea)),
      "SRMR" =  c(render_f(srmr, 2), rndr_rmsea_cfa(srmr))
    )
  )

  colnames(res_lavaan) <- c("Anpassungsmass", "Anforderung")
  res_lavaan <-
    data.frame(Wert = rownames(res_lavaan),
               res_lavaan)


  if (include.model) {
  rslt$model <-
    prepare_output(res_lavaan,
           caption = "Test User Model"
           )
  }


  # label    est    se      z    pvalue ci.lower ci.upper std.lv std.all std.nox
  #Lhr      0.230 0.051  4.483  0.000   0.129    0.330  0.230   0.230   0.230
  #Standardized solution of a latent variable model.
  ldng <- lavaan::standardizedSolution(
    x,
    type = "std.all",
    se = TRUE,
    zstat = TRUE,
    pvalue = TRUE,
    ci = TRUE
  )


  var_type = ifelse(ldng$lhs == ldng$rhs, "Variances", "Latent")
  res_ldng <-
    data.frame(
      model = ifelse(
        ldng$lhs ==  ldng$rhs,
        ldng$rhs,
        paste(ldng$lhs, ldng$op, ldng$rhs)
      ),

      loading = render_f(ldng$est.std, digits = 2),
      h2 = ifelse(ldng$op == "=~",
                  render_f(ldng$est.std ^ 2) , NA),

      SE = render_f(ldng$se, digits = 2),
      z.value = render_f(ldng$z, digits = 2),
      p.value = rndr_P(ldng$pvalue, include.symbol = FALSE),

      stringsAsFactors = FALSE
    )

  if(include.ci){
    res_ldng$CI<- rndr_CI(ldng[c("ci.lower", "ci.upper")]  )

  }
  latent <- res_ldng[which(var_type=="Latent"),]
  varianz <- res_ldng[which(var_type=="Variances"), 1:2]

  if (include.latent) {
   rslt$latent <-  prepare_output(latent,
           caption = "Latent Variables (standardized Solution)")
  }

  if(include.varianz){
   rslt$varianz <- prepare_output(varianz,
           caption = "Variances")
  }

rslt
}


#' Goodness-of-Fit-Index (GFI)
#'
#' Ist vergleichbar mit dem Bestimmtheitsmass in der Regressionsanalyse, also ein Mass fuer die erklaerende Varianz
#' GFI>0.90
#'
#' @noRd
rndr_gfi_cfa <- function(x)
  as.character(cut(x,
                   c(-Inf, 0.89, Inf),
                   c("nicht akzeptabel", "gut")))


#' Adjusted-Goodness-of-Fit-Index (AGFI)
#'
#' Analog wie GFI nur korrigiert durch df und Anzahl an Variablen
#' AGFI>0.90
#'
#' @noRd
rndr_agfi_cfa <- function(x)
  as.character(cut(x,
                   c(-Inf, 0.89, Inf),
                   c("nicht akzeptabel", "gut")))


#' Root-Mean-Square-Error of Approximation (RMSEA)
#'
#' RMSEA<0.05
#'
#' @noRd
rndr_rmsea_cfa <- function(x)
  as.character(cut(
    x,
    c(-Inf, 0.050, 0.08, Inf),#                    c(-Inf,  0.079, Inf),
    c("gut", "akzeptabel", "nicht akzeptabel")#    c("gut", "nicht akzeptabel")))
  ))


#' Chi-Quadrat
#'
#'
#' @noRd
rndr_Chisq_cfa <- function(x, df = 1)
  as.character(cut(
    x / df,
    c(-Inf, 2, 3, Inf),
    c("gut", "akzeptabel", "nicht akzeptabel")
  ))


#' Comparative-Fit-Index (CFI)
#'
#' Wie NFI nur korrigiert durch df und Anzahl an Variablen
#' CFI>0.90
#'
#' @noRd
rndr_cfi_cfa <- function(x)
  as.character(cut(
    x,
    c(-Inf, .950, .970,  Inf),
    c("nicht akzeptabel", "akzeptabel", "gut"),
    right = FALSE
  ))


#' Normed-Fit-Index NFI
#'
#' Vergleicht das Modell mit einem Model bei dem alle Manifesten Variablen un-korreliert angenommen werden
#' NFI>0.90
#'
#' @noRd
rndr_nfi_cfa <- function(x)
  as.character(cut(
    x,
    c(-Inf, .900,  0.950, Inf),
    c("nicht akzeptabel", "akzeptabel", "gut"),
    right = FALSE
  ))


