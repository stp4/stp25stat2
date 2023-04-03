library(stp25stat2)
require(tidyverse)
#?Tbll

#require(stpvers)

lm1 <- lm(breaks ~ wool + tension, data = warpbreaks)
fm1 <- aov(breaks ~ wool + tension, data = warpbreaks)
tk1 <- TukeyHSD(fm1, "tension", ordered = TRUE)





# require(performance)
# model <- lm(mpg ~ wt + cyl, data = mtcars)
# model_performance(model)
#
#
# model <- lm(dist ~ speed, data = cars)
#
# check_heteroscedasticity(model)
# check_model(model)
#
# model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
# model_performance(model)




Tbll(breaks ~ wool + tension, data = warpbreaks )
warpbreaks %>% Tbll_desc(breaks, wool, tension )




# set_opt(  percent = list(style = 2))
# warpbreaks %>% Tbll_desc(breaks, wool, tension )
#
# set_opt(  percent = list(style = 3))
# warpbreaks %>% Tbll_desc(breaks, wool, tension )

set_opt(  percent = list(style = 5))
warpbreaks %>% Tbll_desc(breaks, wool, tension )

warpbreaks %>% Tbll_desc(breaks,  tension, by = ~wool, include.single.value=FALSE)

percent2(gl(2, 8, labels = c("Control", "Treat")))





set_opt(
  percent = list(
    style = 2,
    null_percent_sign =  ' . ',
    digits = 0,
    percentage_str = "",
    include_name = ", n (%)"
  ),

  mean = list(style = 1,
              include_name = ", mean (sd)"),

  median = list(style = 3,
                include_name = ", median (range)"),
    Fstat =list(include.statistic=FALSE),
  table = list(
    stubhead = "Items",
    measure.name.m = "Mittelwert/Prozent",
    measure.name.total = "Summe",
    measure.name.statistics = "P-Werte"
  )
)


warpbreaks %>% Tbll_desc(breaks, wool, tension )

warpbreaks %>%
  Tbll_desc(breaks, tension, by = ~wool,
            include.total=TRUE,
            include.single.value=FALSE)
 warpbreaks %>%
  Tbll_desc(breaks, tension, by = ~wool,
            include.test = TRUE,
            include.total=TRUE )

stp25stat2:::rndr_F(2.98765, 23, 234, .002337356)
get_opt("Fstat", "include.statistic")
#stop()
# Tbll_desc_long ----------------------------------------------------------


mtcars %>% Tbll_desc_long(
  mpg[mean, 1],
  cyl[median],
####  "Hallo",
  disp[0],
  hp[0],
  drat,
  wt,
  qsec[1],
  vs[freq],
  am[freq],
  gear,
  carb,
  include.range = TRUE,
  include.n = TRUE
)





# Tbll_desc ---------------------------------------------------------------


Tbll_desc(
  warpbreaks,
  "H1",
  breaks,
  tension,
  by = ~ wool,
  include.total = TRUE,
  include.n = FALSE,
  include.test = TRUE)


   warpbreaks2 <- stp25tools::Label(warpbreaks,
breaks	=	"The number of breaks",
wool	=	"The type of wool",
tension	=	"The level of tension")
warpbreaks2$tension2 <- as.numeric(warpbreaks2$tension)

warpbreaks2 %>%
  Tbll_desc(breaks + tension ~ wool)
Tbll_desc(
  warpbreaks,
  # "H1",
  breaks,
  tension,
  by = ~ wool,
  #  include.total = TRUE,
  # include.n = FALSE,
  include.test = TRUE,
  include.value = c(breaks = "ES = 26", tension = "OR = .0256")

)

Tbll_desc(
  warpbreaks,
  # "H1",
  breaks,
  tension,
  by = ~ wool,
  #  include.total = TRUE,
  # include.n = FALSE,
  include.test = TRUE,
  include.value = data.frame(ES=1:2, OR= 3:4)




)





# h-test ------------------------------------------------------------------
wilcox.test(mpg ~ vs, mtcars)
Tbll(wilcox.test(mpg ~ vs, mtcars))
Tbll(t.test(mpg ~ vs, mtcars))

 TeaTasting <-
matrix(c(3, 1, 1, 3),
       nrow = 2,
       dimnames = list(Guess = c("Milk", "Tea"),
                       Truth = c("Milk", "Tea")))

 Tbll(fisher.test(TeaTasting, alternative = "greater"))



APA(wilcox.test(mpg ~ vs, mtcars))
APA(t.test(mpg ~ vs, mtcars))

TeaTasting <-
  matrix(c(3, 1, 1, 3),
         nrow = 2,
         dimnames = list(Guess = c("Milk", "Tea"),
                         Truth = c("Milk", "Tea")))
APA(fisher.test(TeaTasting, alternative = "greater"))






# Tbll_reliability --------------------------------------------------------



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







# Tbll_corr ---------------------------------------------------------------


Tbll_corr(breaks ~ tension2, warpbreaks2,  groups = ~ wool)


n <- 2 * 20
e <- rnorm(n)
dat <- stp25aggregate::Label(
  data.frame(
    a = rnorm(n) + e / 2,
    b = rnorm(n) + e,
    c = rnorm(n),
    d = rnorm(n) + e * 10,
    g = gl(2, 20, labels = c("Control", "Treat"))
  ),
  a = "Alpha",
  b = "Beta",
  c = "Gamma"
)


Tbll_corr( ~ a + b + c, dat)
Tbll_corr(a ~ c, dat)
Tbll_corr(a + b + c ~ d, dat)
Tbll_corr(a + b + c ~ d, dat, groups = ~ g)
#'
#'



# Tbll_xtabs --------------------------------------------------------------



Tbll_xtabs( ~ tension + wool, warpbreaks2, include.label = FALSE)



 data(infert, package = "datasets")
infert$case  <- factor(infert$case , 1:0, c("case", "control"))

infert$spontaneous <- factor(infert$spontaneous)
infert$induced2    <- factor(infert$induced == 0)

Tbll_xtabs( ~  case, infert)
Tbll_xtabs( ~ induced2 + case, infert)
Tbll_xtabs( ~ induced + case, infert)
Tbll_xtabs( ~ induced + education, infert)


Tbll_xtabs( ~ induced + education + case,
            infert,
            margin = "case",
            #  add.margins = c("education", "induced"),
            include.count = FALSE)

Tbll_xtabs(
  ~ induced + education + case,
  infert,
  margin = "case",
  add.margins = c("case"),
  include.count = FALSE
)

df<- data.frame(A = c(1,0,0,1,0,1,0,1,1,0,0,0,0,1,1),
B = c(0,0,1,0,1,0,1,1,1,1,1,1,1,0,0)
)


Tbll_xtabs(
  ~ A + B,
  df,

  include.percent = FALSE,
  include.test = TRUE,
  include.diagnostic = TRUE
)




library(MASS)

fit<-loglm(~ Type + Origin, xtabs(~ Type + Origin, Cars93))
Tbll(fit)


data(infert, package = "datasets")
infert$case  <- factor(infert$case ,1:0, c("case", "control") )

infert$spontaneous <- factor(infert$spontaneous)
infert$induced2    <- factor(infert$induced==0)

tab_1<- xtabs(~  case, infert)
tab_2x2<- xtabs(~ induced2 + case, infert)
tab_3x2<- xtabs(~ induced + case, infert)
tab_3x3<- xtabs(~ induced + education, infert)
tab_3x3x2<- xtabs(~ induced + education+case, infert)

#Tbll_xtabs(summary(tab_3x3x2))

(Tbll_xtabs(tab_1, include.test=TRUE))
(Tbll_xtabs(tab_2x2, include.test=TRUE))
(Tbll_xtabs(tab_3x2, include.test=TRUE))
(Tbll_xtabs(tab_3x3, include.test=TRUE))
(Tbll_xtabs(tab_3x3x2, include.test=TRUE))



require(vcd)
data("Arthritis")
tab <- xtabs(~Improved + Treatment, data = Arthritis)
Tbll(assocstats(tab))



# Regression --------------------------------------------------------------



 lm1 <- lm(breaks ~ wool + tension, data = warpbreaks)
 lm2 <- lm(breaks ~ wool * tension, data = warpbreaks)


 Tbll_reg(
  lm1,
  lm2,
  include.p = FALSE,
  include.ci = TRUE,
  include.se=FALSE
)


 Tbll_reg(
   lm2,
   include.p = TRUE,
   include.beta = TRUE,
   include.gof = FALSE,
   include.statistic = TRUE,
   include.stars = TRUE
 )




 counts <- c(18,17,15,20,10,20,25,13,12)
 outcome <- gl(3,1,9)
 treatment <- gl(3,3)
 data.frame(treatment, outcome, counts) # showing data
 lm.D93 <- lm(counts ~ outcome + treatment )
 glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
 Tbll_reg(lm.D93, glm.D93, digits=2)
 Tbll_reg(lm.D93, glm.D93, digits=c(0,2,2,1,1))


#' lm1 <- lm(breaks ~ wool + tension, data = warpbreaks2)


Tbll_reg(lm(breaks ~1, data = warpbreaks2),
         lm1, lm2)

options()$contrasts
 require(MASS)
options(contrasts = c("contr.treatment", "contr.poly"))
house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
#Tbll_reg(house.plr)




require(survival)

fit0 <- survfit(Surv(futime, fustat) ~ 1, data = ovarian)
fit1 <- survfit(Surv(futime, fustat) ~ rx, data = ovarian)
fit2 <- survfit(Surv(futime, fustat) ~ rx + resid.ds, data = ovarian)


#summary(fit1)
Tbll(fit0)
Tbll(fit1)
Tbll(fit2)


Tbll(fit0, include.survival = TRUE, times= c(30*2, 30*6, 30*12))
summary(fit0, times= c(30*2, 30*6, 30*12)) %>% Tbll()

Log.Rang.Test<- survdiff(Surv(futime, fustat) ~ rx, data = ovarian)
Log.Rang.Test %>% Tbll()
APA(Log.Rang.Test)


coxph(Surv(futime, fustat) ~ rx, data = ovarian) %>% Tbll()

cfit1 <- coxph(Surv(futime, fustat) ~ rx , data = ovarian)
cfit2 <- coxph(Surv(futime, fustat) ~ rx + resid.ds, data = ovarian)

broom::glance(cfit2)
Tbll_surv(fit1, fit2, data = ovarian)
Tbll_surv(cfit1, cfit2)
Tbll_reg(cfit1, cfit2, include.gof = FALSE)
Tbll_reg(cfit1, cfit2 )
Tbll(cfit2)


# lmer --------------------------------------------------------------------

library(lmerTest)
require(stp25data)
fit1 <- lm(chol0 ~  ak + rrs0 + med + g, hyper)
fit2 <- glm(chol0 ~ med +   ak +   g + rrs0 , hyper, family = poisson())
fit3 <- lmerTest::lmer(chol0 ~ rrs0 + med +  ak  +  (1|g) , hyper )
Tbll_reg(fit1, fit2, fit3)

Tbll_reg(fit1, fit2, fit3,
         names=c("lm", "glm", "lmer"),
         digits= c(1,2,3,4,5,6,7),

         include.custom=list(
           Wald=c("F(1)=245", "F(2)=245","F(3)=245"),
           Chi=c("X(4)=2.45", "X(5)=24.5","X(6)=24.5")))



# Likert ------------------------------------------------------------------



set.seed(1)
n <- 100
lvs <- c("--", "-", "o", "+", "++")
DF2 <- data.frame(
  Magazines = cut(rnorm(n), 5, lvs),
  Comic.books = cut(rnorm(n), 5, lvs),
  Fiction = cut(rnorm(n), 5, lvs),
  Newspapers = cut(rnorm(n), 5, lvs),
  Geschlecht = cut(rnorm(n), 2, c("m", "f"))
)

x<-Tbll_likert(DF2, Magazines, Comic.books, Fiction, Newspapers, ReferenceZero=2)

stp25output::Output(x)
