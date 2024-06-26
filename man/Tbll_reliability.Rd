% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbll_item_analysis.R, R/tbll_reliability.R
\name{Tbll_item_analysis}
\alias{Tbll_item_analysis}
\alias{Tbll_reliability}
\alias{Tbll_reliability.default}
\alias{Tbll_reliability.psych}
\alias{Tbll_Alpha}
\title{Tbll_reliability}
\usage{
Tbll_item_analysis(
  ...,
  include.label = FALSE,
  include.Sample.SD = FALSE,
  include.Item.total = FALSE,
  include.Alpha = TRUE,
  include.Item.Tot.woi = include.Alpha,
  include.Difficulty = TRUE,
  include.Discrimination = FALSE,
  include.Item.Reliab = TRUE,
  include.Item.Rel.woi = FALSE,
  digits = 2
)

Tbll_reliability(...)

\method{Tbll_reliability}{default}(
  ...,
  include.label = TRUE,
  include.item_statistics = TRUE,
  include.scale_statistics = TRUE,
  include.Alpha = TRUE,
  include.inter.item.correlation = FALSE,
  revcoded = FALSE,
  check.keys = NULL,
  keys = revcoded,
  na.rm = TRUE,
  digits = 2
)

\method{Tbll_reliability}{psych}(x, digits = 2, ...)

Tbll_Alpha(..., type = 1, names = NULL)
}
\arguments{
\item{...}{daten}

\item{include.label}{Labels}

\item{include.Sample.SD}{Standard deviation of the item}

\item{include.Item.total}{(unkorrigierte) Trennschaerfe}

\item{include.Item.Tot.woi}{korrigierte Trennschaerfe Alpha if Item Deleted}

\item{include.Difficulty}{Difficulty Mean of the item (p)}

\item{include.Discrimination}{Itemschwierigkeit Discrimination of the item (u-l)/n}

\item{include.Item.Reliab}{Item reliability index}

\item{include.Item.Rel.woi}{Item reliability index (scored without item)}

\item{digits}{Digits}

\item{include.item_statistics}{Item Statistic}

\item{include.scale_statistics}{Scale Statistic}

\item{include.inter.item.correlation, include.Alpha}{cronbachs Alpha}

\item{revcoded, check.keys, keys}{numeric oder logikal TRUE entspricht check.keys = TRUE}

\item{na.rm}{an psych::alpha}

\item{x, type, names}{... Data, Formula}
}
\value{
data.frame

list ore data.frame
}
\description{
Tbll_item_analysis: stolen from psychometric::item.exam

Sample.SD
Standard deviation of the item

Item.total
Correlation of the item with the total test score

Item.Tot.woi
Correlation of item with total test score (scored without item)

Difficulty
Mean of the item (p)

Discrimination
Discrimination of the item (u-l)/n

Item.Reliab
Item reliability index

Item.Rel.woi
Item reliability index (scored without item)


Item.Criterion
Correlation of the item with the Criterion (y)

Item.Validity
Item validity index

Cronbachs Alpha oder einfach nur α ist ein Maß für die interne Konsistenz einer Skala.
}
\details{
> .9	Exzellent

 > .8	Gut / Hoch

 > .7	Akzeptabel

 > .6	Fragwürdig

 > .5	Schlecht / Niedrig

 < .5	Inakzeptabel

Quelle http://statistikguru.de/spss/reliabilitaetsanalyse/auswerten-und-berichten-2.html

Um die interne Konsistenz zu bestimmen, wurde Cronbachs Alpha für die Subskala positiver Affekt (insgesamt zehn Fragen) berechnet. Die interne Konsistenz war hoch, mit Cronbachs Alpha = .89 für positiven Affekt.


For reliability analysis, Cronbach’s alpha was calculated to assess the internal consistency of the subscale for positive affect, which consists of ten questions. The internal consistency of the questionnaire is satisfying, with Cronbach’s alpha for positive affect = .89.
}
\examples{


TestScores <- stp25tools::get_data(
  "i1 i2 i3 i4 i5 i6 i7 i8 i9 i10    y
  1  1  1  1  1  0  0  1  1   1 2.42
  0  0  0  1  1  0  0  0  1   1 0.25
  1  0  1  1  1  1  1  1  0   1 6.98
  1  1  1  1  1  1  1  0  1   1 6.33
  1  0  1  1  1  1  1  1  0   0 6.44
  1  1  1  1  1  1  1  1  1   1 3.85
  0  0  1  1  1  0  0  0  1   1 4.08
  1  1  1  1  1  1  1  1  1   1 6.85
  1  1  1  1  1  0  1  1  1   1 3.22
  0  0  0  1  1  0  0  0  1   1 1.59
  1  1  1  1  1  0  1  1  1   1 3.08
  0  0  1  1  1  0  0  0  1   1 5.57
  1  1  1  1  1  0  1  1  1   1 3.41
  0  0  0  0  1  0  0  0  1   1 4.51
  0  0  1  1  1  0  0  0  1   1 1.09
  1  0  1  1  1  1  1  1  1   1 6.86
  0  0  0  0  1  1  0  0  1   1 2.94
  0  0  0  0  0  1  0  0  1   1 2.71
  1  1  1  0  0  1  0  0  1   1 2.21
  1  0  1  1  1  1  1  1  1   1 4.01
  0  0  0  0  0  1  0  0  1   0 2.98
  0  0  0  0  0  1  0  0  1   0 2.51
  0  0  0  0  0  1  0  0  1   0 1.26
  0  0  0  0  0  1  0  0  1   1 1.52
  1  1  1  0  0  1  0  0  1   1 6.53
  0  0  0  0  0  1  0  0  1   1 4.08
  0  0  0  1  1  1  0  0  1   1 2.36
  0  0  0  0  0  1  0  0  1   1 3.16
  0  0  0  0  0  1  0  0  1   1 5.15
  0  0  0  0  1  0  0  0  1   1 0.52")

Tbll_item_analysis(TestScores,
    i1, i2, i3, i4, i5, i6, i7, i8, i9, i10,
    by = ~ y)


dat <- data.frame(
  x = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
  y = c(2, 2, 3, 4, 4, 3, 4, 5, 4, 2),
  z = c(3, 5, 1, 2, 3, 2, 3, 4, 4, 5),
  w = c(4, 2, 3, 5, 2, 4, 5, 4, 1, 1)
)

Tbll_reliability(dat,
                 x,
                 y,
                 z,
                 w,
                 revcoded = TRUE,
                 include.item_statistics = FALSE)

m1 <-
  Tbll_reliability(dat,
                   x,
                   y,
                   z,
                   w,
                   revcoded = TRUE,
                   include.item_statistics = FALSE)
m2 <-  Tbll_reliability(dat, x, y, z, w)

Tbll_Alpha(m1, m2)

\donttest{
require(psych)

keys <-
  list(agree=c("-A1","A2","A3","A4","A5"),
       conscientious=c("C1","C2","C3","-C4","-C5"),
       extraversion=c("-E1","-E2","E3","E4","E5"),
       neuroticism=c("N1","N2","N3","N4","N5"),
       openness = c("O1","-O2","O3","O4","-O5"))

scores <- psych::scoreItems(keys,bfi,min=1,max=6) #specify the minimum and maximum values
scores$alpha
summary(scores)
head(scores$scores)

 }

}
