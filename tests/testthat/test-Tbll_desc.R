context("test-Tbll_desc")


test_that("einfache Auswertung", {
  x <- Tbll_desc(
    warpbreaks,
    "H1",
    breaks,
    tension,
    by = ~ wool,
    include.total = TRUE,
    include.n = FALSE,
    include.test = TRUE
  )


  expect_s3_class(x, "data.frame")

  expect_named(x,
               c("Item",
                 "Total",
                 "A",
                 "B",
                 "statistics"))

  expect_equal(x$Item,
               c("H1 ",
                 "breaks (mean)",
                 "tension ",
                 "    L",
                 "    M" ,
                 "    H"))



  r1<-aggregate(
    breaks ~ wool,
    warpbreaks,
    FUN = function(x)
      paste0(round(mean(x), 2), " (", round(sd(x), 2), ")")

  )

  expect_equal(x[[2,3]], r1[1,2])


  expect_equal(x$Total,
               c("",
                 "28.15 (13.20)",
                 "",
                 "33% (18)",
                 "33% (18)",
                 "33% (18)" ))






})


test_that("include.value", {

  x <- Tbll_desc(
    warpbreaks,
    breaks,
    tension,
    wool,
    include.value = data.frame(ES = 1:2, OR = 3:4)
  )

  expect_named(x,
               c("Item", "m",    "ES",   "OR"))

  expect_equal(x$ES, c("",  "1", "2", "",  "",  "",  NA,  "",  "" )
  )


})


test_that("attribut label", {

  attr(warpbreaks$breaks, "label") <- "The number of breaks"
  attr(warpbreaks$tension, "label") <- "The type of wool (A or B)"
  attr(warpbreaks$wool, "label") <- "The level of tension (L, M, H)"

  x <- Tbll_desc(warpbreaks,
                 breaks,
                 tension,
                 wool)

  expect_equal(
    x$Item,
    c(
      "(N) ",
      "The number of breaks (mean)",
      "The type of wool (A or B) ",
      "    L",
      "    M",
      "    H",
      "The level of tension (L, M, H) ",
      "    A",
      "    B"
    )
  )





})


test_that("Tbll_desc_item", {

  df <- data.frame(
    month = rep(1:3, 2),
    student = factor(rep(c("Amy", "Bob"), each = 3)),
    A = c(9, 7, 6, 8, 6, 9),
    B = c(6, 7, 8, 5, 6, 7)
  )

 x<-  Tbll_desc_item( ~ A + B + student, df)

  expect_named(x,
               c(
                 "variable",
                 "n",
                 "M",
                 "SD",
                 "Range",
                 "Skew",
                 "Kurtosi",
                 "Shapiro.Test"
               ))




})


test_that("Tbll_corr", {

  n <- 2 * 20
  e <- rnorm(n)
  dat <-
    data.frame(
      a = rnorm(n) + e / 2,
      b = rnorm(n) + e,
      c = rnorm(n),
      d = rnorm(n) + e * 10,
      g = gl(2, 20, labels = c("Control", "Treat"))
    )

  attr(dat$a, "label") <- "Alpha"
  attr(dat$b, "label") <- "Beta"
  attr(dat$c, "label") <- "Gamma"

  x1 <-
    Tbll_corr(
      ~ a + b,
      dat,
      include.p = FALSE,
      include.stars = FALSE,
      include.mean = TRUE
    )
  x2 <- Tbll_corr(a ~ b, dat)
  r1 <- cor(dat$a, dat$b)
  x3 <-  Tbll_corr(a + b + c ~ d, dat, type =  "spearman")
  x4 <-  Tbll_corr(a + b + c ~ d, dat, groups = ~ g)

  expect_named(x1,
               c("Source", "M (SD)", "(1)", "(2)"))
  expect_named(x2,
               c("Characteristics", "b_N",  "b_r", "b_p.value"))

  expect_equal(as.numeric(x1[[1, 4]]),  round(r1, 2))
  #expect_equal(x1[1, 4], x2[1, 3])
  expect_equal(x1[[1, 4]], x2[[1, 3]])

  expect_equal(as.numeric(x3[[3]]),
               round(c(
                 cor(dat$a, dat$d,  method = "spearman"),
                 cor(dat$b, dat$d,  method = "spearman"),
                 cor(dat$c, dat$d,  method = "spearman")
               ), 2))

  expect_named(
    x4,
    c(
      "Characteristics",
      "Control_N",
      "Control_r",
      "Control_p.value",
      "Treat_N",
      "Treat_r",
      "Treat_p.value"
    )
)

})


test_that("test helper-sig-test works", {
  dat <- data.frame(
    m1 = c(1, 2, 1, 3, 1, 4, 1, 1,
           3, 5, 3, 1, 3, 6, 3, 1),
    geschl = gl(2, 8, labels = c("m", "f"))
  )
  dat$m2 <- cut(dat$m1, 2)

  expect_equal(stp25stat2:::conTest(m1  ~ geschl, dat),
               c("Wilcoxon-Test" = "F(1, 14)=3.20, p=.095"))
  expect_equal(stp25stat2:::catTest(m1  ~ geschl, dat),
               "Error wrong formula!")


  expect_error(
    stp25stat2:::ordTest(m1  ~ geschl, dat)
  )
  # expect_equal(
  #   stp25stat2:::ordTest(m1  ~ geschl, dat),
  #   c("Logistic Regression" = "LR(1)=3.14, p=.077")
  # )


  expect_equal(
    stp25stat2:::spearmanTest2(m1  ~ geschl, dat),
    c("Wilcoxon-Test" = "F(1, 14)=3.20, p=.095")
  )
  expect_equal(stp25stat2:::TTest2(m1  ~ geschl, dat),
               c("T-Test" = "T(12)=-1.87, p=.086"))
  expect_equal(stp25stat2:::Aov2(m1  ~ geschl, dat),
               c("ANOVA" =  "F(1, 14)=3.49, p=.083"))
  expect_equal(
    stp25stat2:::KruskalTest2(m1  ~ geschl, dat),
    c("Kruskal-Wallis-Test" = "H(1)=2.79, p=.095")
  )
  expect_equal(stp25stat2:::WilkoxTest2(m1  ~ geschl, dat),
               c("Wilcoxon-Test" = "U=17.00, p=.106"))
  expect_equal(stp25stat2:::chisqTest2(m1  ~ geschl, dat),
               "Error wrong formula!")
  expect_equal(
    stp25stat2:::chisqTest2(~ m1 + geschl, dat),
    c("Pearson Chi-squared" = "X2(5)=7.09, p=.214")
  )
  expect_equal(stp25stat2:::fisherTest2(~ m1 + geschl, dat),
               "wrong dim for fisher-test")
  expect_equal(
    stp25stat2:::fisherTest2(~ m2 + geschl, dat),
    c("Fisher Exact Test" = "OR=2.21, p=1.000")
  )
})


test_that("test helper-prct_or_mean", {

  x1 <- stp25stat2:::prct_or_mean(1:10, 2, "numeric", "A",
                                    include.single.value = FALSE)
  x2 <- stp25stat2:::prct_or_mean(1:10, 2, "numeric", "A",
                                    include.single.value = TRUE)
  expect_equal(x1,
               data.frame(
                 Item = "A",
                 lev = "(mean)",
                 n   = "10",
                 m   = "5.50",
                 sd  = "3.03"
               ))


  expect_equal(x2,
               data.frame(
                 Item = "A",
                 lev = "(mean)",
                 n   = "10",
                 m   = "5.50 (3.03)"
               ))
})


test_that("Tbll_desc_multi", {
  dat <- data.frame(
    Hotel =  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    Pension = c(1, 1, 0, 0, 0, 1, 1, 1, 1, 1),
    Hostel = c(1, 1, 0, 1, 1, 0, 0, 1, 0, 1),
    Ferienwohnung = c(1, 1, 1, 1, 0, 0, 0, 1, 0, 0),
    Airbnb = c(0, 0, 0, 0, 0, 1, 0, 1, 1, 1),
    Campingplatz = c(0, 0, 0, 1, 0, 1, 1, 0, 0, 0),
    Couchsurfing =c(0,0,0,0,0, 0, 1, 0, 0, 0)

  )

  x<- Tbll_desc_multi(dat,
                      Hotel,
                      Pension,
                      Hostel,
                      Ferienwohnung,
                      Airbnb,
                      Campingplatz,
                      Couchsurfing)

  expect_named(x, c( "Item", "m"))


  expect_equal(x$m,
               c("10",
                 "100% (10)",
                 "70% (7)",
                 "60% (6)",
                 "50% (5)" ,
                 "40% (4)",
                 "30% (3)",
                 "10% (1)"))

})








test_that("Auto Test works", {

  dat <- data.frame(
    m1 = c(1, 2, 1, 3, 1, 4, 1, 1,
           3, 5, 3, 1, 3, 6, 3, 1),
    geschl = gl(2, 8, labels = c("m", "f"))
  )
  dat$m2 <- cut(dat$m1, 2)


  expect_equal(conTest(m1  ~ geschl, dat),
               c("Wilcoxon-Test" = "F(1, 14)=3.20, p=.095"))

  expect_equal(catTest(m1  ~ geschl, dat) , "Error wrong formula!")
  #ordTest(m1  ~ geschl, dat)
  #rms::lrm(m1  ~ geschl, dat)

  expect_equal(spearmanTest2(m1  ~ geschl, dat),
               c("Wilcoxon-Test" = "F(1, 14)=3.20, p=.095"))

  expect_equal(TTest2(m1  ~ geschl, dat),
               c("T-Test" = "T(12)=-1.87, p=.086"))

  expect_equal(Aov2(m1  ~ geschl, dat),
               c(ANOVA = "F(1, 14)=3.49, p=.083"))

  expect_equal(KruskalTest2(m1  ~ geschl, dat),
               c("Kruskal-Wallis-Test" = "H(1)=2.79, p=.095"))

  expect_equal(WilkoxTest2(m1  ~ geschl, dat),
               c("Wilcoxon-Test" = "U=17.00, p=.106"))

  expect_equal(chisqTest2(m1 ~ geschl, dat), "Error wrong formula!")

  expect_equal(chisqTest2( ~ m1 + geschl, dat),
               c("Pearson Chi-squared" =  "X2(5)=7.09, p=.214"))

  expect_equal(fisherTest2( ~ m1 + geschl, dat), "wrong dim for fisher-test")

  expect_equal(fisherTest2( ~ m2 + geschl, dat),
               c("Fisher Exact Test" = "OR=2.21, p=1.000"))

})
