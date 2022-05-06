context("test-anova")


test_that("anova Tbll works", {
  fm1 <- aov(breaks ~ wool + tension, data = warpbreaks)
  x <- Tbll(fm1)
  expect_equal(names(x),
               c("term", "sumsq", "df", "statistic",  "eta.sq.part", "p.value"))

  expect_equal(x$sumsq,
               c("450.67",  "2034.26", "6747.89"))

})


test_that("anova error mit interaction", {
  npk.aov <- aov(yield ~ block + N + P + K, npk)
  expect_equal(extract_param(npk.aov)$sumsq,
               summary(npk.aov)[[1]][[2]])

})


test_that("TukeyHSD works", {
  fm1 <- aov(breaks ~ wool + tension, data = warpbreaks)
  tk1 <- TukeyHSD(fm1, "tension", ordered = TRUE)
  # fm1_split <-  summary(fm1,
  #                       split = list(tension = list(
  #                         M = 1,  H = 3, L = 2
  #                       )),
  #                       expand.split = FALSE)
  #Tbll(fm1)
  rst <- Tbll(tk1, digits = 3)
  expect_equal(
    names(rst),
    c(
      "term",
      "contrast",
      "null.value",
      "estimate",
      "conf.low",
      "conf.high",
      "adj.p.value"
    )
  )
  expect_equal(rst$estimate,
               c("4.722",  "14.722", "10.000"))
})


test_that("anova-lda works", {

  n <- 3 * 2 * 3 *10
  set.seed(n)
  x <- rnorm(n)
  DF <-
    data.frame(
      JOB = gl(3, n, labels = c(
        "customer service", "mechanic", "dispatcher"
      )),
      OUTDOOR = rep(seq(1, 50, length.out = 3),n/3)  + x,
      SOCIAL =   rep(seq(1, 20, length.out = 6),n/6) + x,
      CONSERVATIVE = rep(seq(1, 30, length.out = 9), n/9) + x
    )

  fit <- MASS::lda(JOB ~ OUTDOOR + SOCIAL + CONSERVATIVE, data = DF)
  res <- Tbll(fit)

  expect_equal(names(res),
               c("mean",   "scal",   "svd" ,   "cTab" ,  "cTotal"))

  expect_equal(res$cTotal[[2]],
               c("32.8", "32.2", "33.9", "33.0")
  )

})


test_that("manova works", {
  set.seed(1)
  npk2 <- within(npk, foo <- rnorm(24))
  npk2 <- within(npk2, foo2 <- rnorm(24))

  npk2.aov <-
    manova(cbind(yield, foo, foo2) ~ block + N * P * K, npk2)

  # broom::tidy(npk2.aov)
  # parameters::model_parameters(npk2.aov, eta_squared = TRUE )

  expect_warning( rslt <- Tbll(npk2.aov, eta_squared = TRUE))
  expect_equal(names(rslt),
               c("term", "pillai", "df", "statistic", "eta.sq.part", "p.value"))
  expect_equal(rslt$statistic,
               c("1.48", "7.09", "0.77", "2.66", "1.45", "1.01", "3.25", ""))

})

