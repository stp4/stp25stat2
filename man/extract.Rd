% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-lda.R, R/extract.R, R/extract_gof.R,
%   R/extract_manova.R, R/extract_mediate.R, R/extract_meta.R,
%   R/extract_param.R, R/extract_residuals.R, R/tbll_effect.R
\name{tbll_extract.lda}
\alias{tbll_extract.lda}
\alias{extract}
\alias{tbll_extract}
\alias{tbll_extract.default}
\alias{tbll_extract.anova}
\alias{tbll_extract.data.frame}
\alias{tbll_extract.lm}
\alias{tbll_extract.aov}
\alias{tbll_extract.manova}
\alias{tbll_extract.TukeyHSD}
\alias{tbll_extract.aovlist}
\alias{tbll_extract.summary.aov}
\alias{tbll_extract.step}
\alias{tbll_extract.table}
\alias{tbll_extract.summary.table}
\alias{tbll_extract.htest}
\alias{tbll_extract.pairwise.htest}
\alias{tbll_extract.likert}
\alias{tbll_extract.matchit}
\alias{tbll_extract.summary.matchit}
\alias{tbll_extract.principal}
\alias{tbll_extract.confusionMatrix}
\alias{tbll_extract.epi.tests}
\alias{tbll_extract.roc}
\alias{tbll_extract.assocstats}
\alias{tbll_extract.list}
\alias{tbll_extract.loglm}
\alias{extract_gof}
\alias{extract.manova}
\alias{tbll_extract.mediate}
\alias{tbll_extract.summary.mediate}
\alias{tbll_extract.meta}
\alias{extract_param}
\alias{extract_param_aov}
\alias{extract_effsize}
\alias{extract_etaSqr}
\alias{extract_partial_residuals}
\alias{extract_effect}
\alias{tbll_extract.eff}
\title{Extract}
\usage{
\method{tbll_extract}{lda}(
  x,
  newdata = NULL,
  include.means = FALSE,
  include.scal = TRUE,
  include.cTab = TRUE,
  include.svd = TRUE,
  digits = 2,
  ...
)

tbll_extract(...)

\method{tbll_extract}{default}(x, ...)

\method{tbll_extract}{anova}(x, include.eta = TRUE, ...)

\method{tbll_extract}{data.frame}(x, ...)

\method{tbll_extract}{lm}(...)

\method{tbll_extract}{aov}(...)

\method{tbll_extract}{manova}(...)

\method{tbll_extract}{TukeyHSD}(x, digits = 2, ...)

\method{tbll_extract}{aovlist}(x, ...)

\method{tbll_extract}{summary.aov}(x, ...)

\method{tbll_extract}{step}(x, include.se = FALSE, include.df = FALSE, ...)

\method{tbll_extract}{table}(...)

\method{tbll_extract}{summary.table}(x, ...)

\method{tbll_extract}{table}(...)

\method{tbll_extract}{htest}(x, ...)

\method{tbll_extract}{pairwise.htest}(x, caption = paste(x$data.name, x$method), ...)

\method{tbll_extract}{likert}(...)

\method{tbll_extract}{matchit}(x, ...)

\method{tbll_extract}{summary.matchit}(x, digits = 3, ...)

\method{tbll_extract}{principal}(x, ...)

\method{tbll_extract}{confusionMatrix}(x, digits = 2, Positive_Class = x$positiv, ...)

\method{tbll_extract}{epi.tests}(
  x,
  type = c(se = "Sensitivity", sp = "Specificity", pv.pos = "Positive predictive value",
    pv.neg = "Negative predictive value", p.tpdn = "False T+ proportion for true D-",
    p.tndp = "False T- proportion for true D+", p.dntp = "False T+ proportion for T+",
    p.dptn = "False T- proportion for T-", diag.ac = "Correctly classified proportion"),
  digits = 2,
  ...
)

\method{tbll_extract}{roc}(
  x,
  digits = 2,
  type = c(threshold = "Threshold value", specificity = "Specificity", sensitivity =
    "Sensitivity", accuracy = "Accuracy", tpr = "True Positive Rate", fpr =
    "False Positive Rate", tnr = "True Negative Rate", fnr = "False Negative Rate", fdr =
    "False Discovery Rate"),
  ...
)

\method{tbll_extract}{assocstats}(x, ...)

\method{tbll_extract}{list}(x, ...)

\method{tbll_extract}{loglm}(x, include.ll.ratio = TRUE, include.pearson = TRUE, ...)

extract_gof(
  x,
  include.ftest = TRUE,
  include.loglik = FALSE,
  include.minus.LL = include.loglik,
  include.r = FALSE,
  include.heteroskedasticity = TRUE,
  include.durbin = TRUE,
  include.levene = FALSE,
  include.bartlett = FALSE,
  include.vif = FALSE,
  include.sigma = FALSE,
  include.rmse = FALSE,
  include.aic = TRUE,
  include.bic = TRUE,
  include.residual = TRUE,
  include.normality = FALSE,
  include.multicollin = include.vif,
  include.deviance = TRUE,
  ...
)

extract.manova(
  x,
  test = "Pillai",
  include.manova = !is.null(test),
  include.univariate = TRUE,
  include.eta = FALSE,
  include.order = TRUE,
  ...
)

\method{tbll_extract}{mediate}(x, ...)

\method{tbll_extract}{summary.mediate}(x, digits = 2, ...)

\method{tbll_extract}{meta}(
  x,
  digits = 2,
  include.I2 = TRUE,
  include.Q = TRUE,
  include.tau = TRUE,
  include.H = TRUE,
  ...
)

extract_param(
  x,
  include.b = TRUE,
  include.se = TRUE,
  include.beta = FALSE,
  include.ci = FALSE,
  include.odds = FALSE,
  include.odds.ci = if (include.odds & include.ci) TRUE else FALSE,
  include.statistic = TRUE,
  include.p = TRUE,
  include.stars = FALSE,
  include.df = FALSE,
  include.effects = c("ran_pars", "fixed"),
  include.eta = TRUE,
  include.sumsq = TRUE,
  include.meansq = FALSE,
  include.omega = FALSE,
  include.power = FALSE,
  conf.int = TRUE,
  conf.level = 0.95,
  conf.method = "Wald",
  fix_format = FALSE,
  digits.param = 3,
  digits.odds = 2,
  digits.test = 2,
  digits.beta = 2,
  format = "fg",
  conf.style.1 = FALSE,
  ...
)

extract_param_aov(
  x,
  include.eta = FALSE,
  include.sumsq = TRUE,
  include.meansq = FALSE,
  include.omega = FALSE,
  include.power = FALSE,
  fix_format = FALSE,
  ...
)

extract_effsize(x, ...)

extract_etaSqr(x, type = 2, anova = FALSE, ...)

extract_partial_residuals(x, ...)

extract_effect(..., names = NULL, transformation, xlevels = list())

\method{tbll_extract}{eff}(x, ...)
}
\arguments{
\item{x}{Objekt oder Formel}

\item{digits}{in extract.matchit}

\item{...}{weitere Optionen}

\item{include.eta}{die Manova wird ueber heplots::etasq berechnet und die anova mit den SS eta2=SS/SS_total}

\item{include.se, include.df}{in extract.step}

\item{Positive_Class, type, test, note, include.I2, include.Q, include.tau, include.H, include.sumsq, include.meansq, include.omega, include.power, conf.int, fix_format, digits.param, digits.odds, digits.test, digits.beta, format, conf.style.1, anova}{description}

\item{include.ll.ratio, include.pearson}{in extract.loglm}

\item{include.ftest, include.vif, include.heteroskedasticity, include.durbin, include.levene, include.bartlett}{GOF for lm}

\item{include.loglik, include.deviance, include.minus.LL}{GOF for glm}

\item{include.r, include.sigma, include.rmse, include.aic, include.bic, include.residual, include.normality, include.multicollin}{GOF for lm and glm}

\item{test}{The name of the test statistic c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")}

\item{include.univariate, include.manova}{logical}

\item{include.order}{reorder univariate results by F-values}

\item{include.b, include.beta, include.ci, include.se, include.odds, include.odds.ci, include.statistic, include.p, include.stars, include.df, include.effects, conf.level, conf.method}{Parameter fuer die Regress-Tabellen}

\item{names}{erste Spalte}

\item{transformation}{an effects::allEffect}

\item{xlevels}{an effects::allEffects()}

\item{term}{effects: x-var}

\item{response}{effects: y-var}
}
\value{
data.frame

data.frame

data.frame(term, level, fit, se,lower, upper, value)
Ausgabe der Effekte als dataframe zum erstellen von Grafiken
}
\description{
Turn an object into a tidy table

MANOVA: extract.manova(x, , test="Wilks") test : "Wilks", "Pillai"

Causal Mediation Analysis:
 stolen from mediate {mediation} and psych {mediate}

tbll_extract.summary.mediate: extract Output from mediate:::summary.mediate

tbll_extract.meta: Ausgabe von print.meta als tabelle

Pearson's r correlation Small 0.2,  Medium 0.5, Large 0.8
r2 coefficient of determination Small 0.04, Medium 0.25, Large 0.64

effects:   Extract partial effects obtained with allEffects()

Extrahiert die Effectte (magrinal-means) mit effects::allEffects
alternative ist  marginaleffects::marginal_means aber hier können nur
 Factoren untersucht werden.
}
\details{
Quelle: http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3444174/pdf/i1949-8357-4-3-279.pdf

 Gestolen von https://cran.r-project.org/web/packages/effsize/effsize.pdf

Irgendas ist hier faul!!!
}
\examples{

\donttest{
summary(lm1 <- lm(Fertility ~ ., data = swiss))
slm1 <- stats::step(lm1)
tbll_extract(slm1)
 require(lmerTest)
 m <- lmerTest::lmer(
  Informed.liking ~ Product * Information * Gender +
  (1 | Consumer) + (1 | Product:Consumer),
  data = ham
 )
# elimination of non-significant effects
 s <- lmerTest::step(m)

 tbll_extract(s)

}

a <- letters[1:3]
tbll_extract(summary(table(a, sample(a))))


\donttest{
attach(airquality)
Month <- factor(Month, labels = month.abb[5:9])

## These give warnings because of ties :
tbll_extract(pairwise.wilcox.test(Ozone, Month))

}


\donttest{
# Sensitivität: richtig positive Rate eines Tests
# Spezifität: richtig-negative Rate eines Tests

tab<-matrix(c(94,40,39,40), ncol=2, byrow = TRUE)
tbll_extract( epiR::epi.tests(tab) )

}


\donttest{
require(vcd)
data("Arthritis")
tab <- xtabs(~Improved + Treatment, data = Arthritis)
tbll_extract(assocstats(tab))
}

\donttest{
require(MASS)
minn38a <- xtabs(f ~ ., minn38)
fm <- loglm(~ 1 + 2 + 3 + 4, minn38a)  # numerals as names.
# deviance(fm)
tbll_extract(fm)

}
\donttest{
#- manova ---------------------------------------------

## Set orthogonal contrasts.
op <- options(contrasts = c("contr.helmert", "contr.poly"))
## Fake a 2nd response variable
npk2 <- within(npk, foo <- rnorm(24))
npk2 <- within(npk2, foo2 <- rnorm(24))
npk2.aov <- manova(cbind(yield, foo, foo2) ~ block + N * P * K, npk2)

stp25stat2:::extract.manova(npk2.aov) #wilks
stp25stat2:::extract.manova(npk2.aov, "Pillai")

#npk2.aovE <- manova(cbind(yield, foo) ~  N*P*K + Error(block), npk2)
# stp25stat2:::extract.manova(npk2.aovE)



 DF<- stp25tools::get_data(
"C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25data/extdata/manova.sav"
)

#information from
DF$group <- factor(DF$group , 1:3, Cs("website", "nurse ", "video tape" ))

z<- as.matrix(DF[,-1])
fit1<- manova(z ~ DF$group )
stp25stat2:::extract.manova(fit1)

summary(fit1)$Eigenvalues

# SPSS
# Multivariate Tests of Significance (S = 2, M = 0, N = 13 )
#
# Test Name       Value  Approx. F Hypoth. DF   Error DF  Sig. of F
#
# Pillais          .48    3.02       6.00      58.00       .012
# Hotellings       .90    4.03       6.00      54.00       .002
# Wilks            .53    3.53       6.00      56.00       .005
# Roys             .47
# Note.. F statistic for WILKS' Lambda is exact.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Univariate F-tests with (2,30) D. F.
#
# Variable   Hypoth. SS   Error SS Hypoth. MS   Error MS          F  Sig. of F
#
# USEFUL       52.92424  293.96544   26.46212    9.79885    2.70053       .083
# DIFFICUL      3.97515  126.28728    1.98758    4.20958     .47216       .628
# IMPORTAN     81.82969  426.37090   40.91485   14.21236    2.87882       .072
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Eigenvalues and Canonical Correlations
#
# Root No.    Eigenvalue        Pct.   Cum. Pct.  Canon Cor.
#
# 1          .892      99.416      99.416        .687
# 2          .005        .584     100.000        .072
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


}




\donttest{
 library(meta)
data(Olkin1995)
head(Olkin1995)
m1 <- metabin(ev.exp, n.exp, ev.cont, n.cont,
              data = Olkin1995, subset = c(41, 47, 51, 59),
              studlab = paste(author, year),
              method = "Inverse")
print(m1, digits = 1, digits.Q = 1)
Tbll(m1)

}
\donttest{
fit <- lm(Sepal.Width ~ Petal.Length + Petal.Width, iris)
# broom::tidy(fit)
# broom::glance(fit)
# summary(fit)
extract_param(fit)

}


op <- options(contrasts = c("contr.helmert", "contr.poly"))
## as a test, not particularly sensible statistically
npk.aovE <- aov(yield ~  N * P * K + Error(block), npk)
require(stp25stat2)
Tbll( npk.aovE, include.eta = TRUE )

## Intern wird model_parameters angewendet
parameters::model_parameters(npk.aovE, effectsize_type = "eta")

options(op)

\donttest{
# etaSquared

fit1<-lm(y1~x1, anscombe)

# extract_etaSqr(aov (y1~x1, anscombe), anova=TRUE)
# extract_etaSqr(fit1, anova=TRUE)
extract_etaSqr(fit1)
}

\donttest{
 require(effects)

 data(Prestige, package = "carData")
 mod <- lm(log(prestige) ~ income:type + education, data=Prestige)
 eff = effect("education", mod, partial.residuals=TRUE)
 dat<- extract_partial_residuals(eff)

# library(ggplot2)
# library(gridExtra)

# https://stackoverflow.com/questions/43950459/use-ggplot-to-plot-partial-effects-obtained-with-effects-library

 # g <- ggplot(dat$fit, aes(x = education, y = fit)) +
 # theme_bw() +
 # geom_line(size = 1) +
 # geom_point(data = dat$residuals, aes(x = education , y =  partial.residuals), shape = 1, col = "blue", size = 2) +
 # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
 #  geom_smooth(data =  dat$residuals, aes(x = education, y =  partial.residuals),   method = "loess", span = 2/3, linetype = "dashed", se = FALSE)
}

set.seed(1)

g<- gl(2, 8, labels = c("Control", "Treat"))
x<- rnorm(2*8)
y<- rnorm(2*8)/10 + x
fit1<-lm(y~x)
fit2<-lm(y~x+g)

extract_effect(fit1, xlevels=list(x=c(-3,0,1)))
extract_effect(fit1, fit2, xlevels=list(x=c(-1,0,1)))


\donttest{
require(effects)
fit1 <-
  lm (Sepal.Length ~ Sepal.Width * Species + Petal.Width, data = iris)
fit2 <-
  lm (log(Sepal.Length) ~ Sepal.Width * Species + Petal.Width, data = iris)

x1 <- allEffects(fit1)
x2 <- allEffects(fit2,
                 transformation = list(link = log, inverse = exp))
x3 <- effect("Petal.Width",  fit1)
x4 <- effect("Petal.Width",  fit2,
             transformation = list(link = log, inverse = exp))
Tbll(x1)
Tbll(x3)
Tbll(x2)
Tbll(x4)

}

}
