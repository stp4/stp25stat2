#' CFA und SEM
#' mehr unter http://www.understandingdata.net/2017/03/22/cfa-in-lavaan/
#'
#'
#' Loading ~ standartisierte Estimate und
#'
#' Communality ~ quadrierte Ladung
#'
#'
#' Chi-Quadrat-Wert
#'
#' ML:  Validitaet des Models H0: empirische Kovarianz entspricht modelltheoretischer Kovarianz
#' Chi-Quadrat/df moeglichst klein (Chi-Quadrat/df<2.5 oder p<0.100)
#' Ist nur zuverlaessig wenn Normalverteilung und ausreichend grosse Stichprobe gegeben ist.
#'
#' Model test Baseline model Chi-Quadrat Null-Modell wenn signifikant dann besteht die Gefahr einer Fehl-Spezifikation
#'
#' Goodness-of-Fit-Index (GFI)
#'
#' Ist vergleichbar mit dem Bestimmtheitsmass in der Regressionsanalyse, also ein Mass fuer die erklaerende Varianz
#' GFI>0.90
#' Adjusted-Goodness-of-Fit-Index (AGFI)
#' Analog wie GFI nur korrigiert durch df und Anzahl an Variablen
#' AGFI>0.90
#'
#' Normed-Fit-Index NFI
#'
#' Vergleicht das Modell mit einem Model bei dem alle Manifesten Variablen un-korreliert angenommen werden
#' NFI>0.90
#' Comparative-Fit-Index
#'
#' Wie NFI nur korrigiert durch df und Anzahl an Variablen
#' CFI>0.90
#'
#' Root-Mean-Square-Error of Approximation (RMSEA)
#' RMSEA<0.05
#'
#' Backhaus Multivariate Analysemethoden 11 AuflageSeite 383
#' Moosbrugger, Kelava 2012 Testtheorie 2. Auflage Seite 339
#'
#'
#' Regarding the CFA we used goodness of fit indices to evaluate the model.
#'  Based on the literature, the CFI and TLI cut-off scores should be above 0.9.
#'  We got a CFI value of 0.75, and a TLI value of 0.73, which are not good fit
#'  indices for the internal validity. The value of RMSEA is 0.06, and SRMR
#'  is 0.07, which is a little above the cut-of score of .05. All of the
#'  estimate coefficients loadings are significant. However, some of them have
#'  a negative sign which indicate that they need to be recoded. All variances
#'  have a positive sign which is good. The first factor (Extraversion) has five
#'  negative items; E2, E4, E6, E8, and E10. The second factor (Neuroticism)
#'  has two negative items; N2 and N4. The third factor (Agreeableness) has
#'  six negative items; A2, A4, A6, A8, A9, and A10. The fourth factor,
#'  which is Conscientiousness, has four negative items; C2, C4, C6, and C8.
#'  The fifth factor, which is Openness to experience, has three negative
#'  items; O2, O4, and O6. We should look at the item content to decide if
#'  they need to be recoded, deleted, or changed. According to Kline (2005),
#'  negative variance estimates or loadings are unacceptable.
#'
#' Quelle: https://sites.education.miami.edu/statsu/2020/10/12/steps-of-conducting-confirmatory-factor-analysis-cfa-in-r/
#'
#' @name Tbll_cfa
#'
#' @examples
#'
#' require(stp25data)
#' head(fkv)
#'
#' #library(arm)
#' # windows(5,5)
#' # corrplot(fkv, abs=TRUE, n.col.legend=7)#  corrplot {arm}
#' #
#' # nicht mehr vorhanden    Principal2(fkv, 5, cut=.35)
#'
#' library(lavaan)
#' library(semPlot)
#'
#'
#' Model<-'
#' Verarbeitung =~ F5+F16+F22+F9+F26+F6+F35+F33+F12+F34+F4
#' Coping =~ F7+F8+F17+F14+F15+F18+F19+F1+F13+F20
#' Vertrauen =~ F28+F27+F31+F29
#' Religion =~F21+F25+F30+F23+F24
#' Distanz =~F3+F2+F10+F11
#'
#' '
#' fit.Lavaan <- sem( Model, data=fkv)
#' summary(fit.Lavaan)
#' # parameterEstimates(fit.Lavaan)
#' # Est <- parameterEstimates(fit.Lavaan, ci = FALSE, standardized = TRUE)
#' # #fitMeasures(fit.Lavaan, c("chisq", "df", "pvalue", "cfi", "rmsea"))
#' # #round( inspect(fit.Lavaan,"r2")  ,2)
#' # #parTable(fit.Lavaan)
#' # #show(fit.Lavaan)
#' # anova(fit.Lavaan)
#'
#' #semPaths(fit.Lavaan, "std", rotation=2, title = FALSE)
#' #title("Std", line = 3)
#'
#'
#'
#'
#'
#'
#'
NULL



