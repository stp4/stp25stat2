context("test-lm-glm-regression")


#' Seit R version 4.0.0 (2020-04-24) funktionieren die Formeln
#' I(sr < 10 ~ pop15 + pop75 + dpi)
#' nicht mehr!
#'
#' poisson_sim
require(stp25data)


DF <- LifeCycleSavings
DF$sr10 <- DF$sr < 10

test_that("lm extract_param estimate same as coef", {
  fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
  fit1 <- update(fit0, . ~ . + pop15)
  fit2 <- update(fit1, . ~ . + pop75)
  fit3 <- update(fit2, . ~ . + dpi)
  fit4 <- update(fit3, . ~ . + cut(ddpi, 3))


  expect_equivalent(extract_param(fit0)[2],
                    broom::tidy(fit0)[2])

  fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
  fit1 <- update(fit0, . ~ . + pop15)
  fit2 <- update(fit1, . ~ . + pop75)
  fit3 <- update(fit2, . ~ . + dpi)


  expect_equal(as.vector(coef(fit2)),
               extract_param(fit2)$estimate)

  expect_equal(as.vector(coef(fit3)),
               extract_param(fit3)$estimate)

  expect_equal(as.vector(coef(fit4)),
               extract_param(fit4)$estimate)


})

test_that("lm extract_param include._", {
  fit3 <- lm(sr ~ pop15+ pop75+ dpi, data = LifeCycleSavings)

  res <- extract_param(
    fit3,
    include.b = FALSE,
    include.se = FALSE,
    include.beta = TRUE,
    include.ci = TRUE,
    include.odds = TRUE,
    include.odds.ci = TRUE,
    include.statistic = FALSE,
    include.p = FALSE,
    include.stars = TRUE
  )
  expect_equal(names(res),
               c("term",
                 "conf.high",
                 "conf.low",
                 "beta",
                 "stars"))
})

test_that("lm regression_table", {
  fit1 <- lm(chol0 ~ ak + med + rrs0 + g, hyper)
  fit2 <- lm(chol0 ~  g + ak+rrs0+ med  , hyper)
  fit3 <- lm(chol0 ~ rrs0 +ak +  g+ med , hyper)

  coefs <- regression_table(
    list(fit1,
         fit2,
         fit3), include.se=FALSE, output = FALSE)


  expect_equal(coefs[[3]][13], c( "174") )

})

test_that("lm Tbll_reg vs Tbll_reg_long", {
  fit4 <-
    lm(sr ~ pop15 + pop75 + dpi + cut(ddpi, 3), data = LifeCycleSavings)
  x1 <- Tbll_reg(fit4)
  x2 <- Tbll_reg_long(fit4)

  expect_equal(x1[[1]][1:6],
               x2[[1]][1:6])

  expect_equal(x1[[2]][1:6],
               c("29.4***",
                 "-0.456**",
                 "-1.51",
                 "-0.000494" ,
                 "1.62",
                 "2.63"))

})




test_that("glm extract_param estimate same as coef", {



  fit <-
    glm(sr10 ~ pop15 + pop75 + dpi,
        data = DF,
        family = binomial)

  expect_equal(as.vector(coef(fit)),
               broom::tidy(fit)$estimate)

  expect_equal(extract_param(fit)$estimate,
               broom::tidy(fit)$estimate)

  fit5 <-
    glm(num_awards ~ prog + math, poisson_sim, family =  poisson())


  expect_equal(extract_param(fit5)$estimate,
               broom::tidy(fit5)$estimate)


})


test_that("glm extract_param include._", {
  fit <-
    glm(sr10 ~ pop15 + pop75 + dpi,
        data = DF,
        family = binomial)
  fit5 <-
    glm(num_awards ~ prog + math, poisson_sim, family =  poisson())


  res <- extract_param(
    fit,
    include.b = FALSE,
    include.se = FALSE,
    include.beta = TRUE,
    include.ci = TRUE,
    include.odds = TRUE,
    include.odds.ci = TRUE,
    include.statistic = FALSE,
    include.p = FALSE,
    include.stars = TRUE
  )
  expect_equal(
    names(res),
    c(
      "term",
      "conf.high",
      "conf.low",
      "odds",
      "odds.conf.low",
      "odds.conf.high",
      "stars"
    )
  )

  res5 <- extract_param(
    fit,
    include.b = FALSE,
    include.se = FALSE,
    include.beta = TRUE,
    include.ci = TRUE,
    include.odds = TRUE,
    include.odds.ci = TRUE,
    include.statistic = FALSE,
    include.p = FALSE,
    include.stars = TRUE
  )
  expect_equal(
    names(res5),
    c(
      "term",
      "conf.high",
      "conf.low",
      "odds",
      "odds.conf.low",
      "odds.conf.high",
      "stars"
    )
  )
})


test_that("glm einfache Auswertung", {
  fit <-
    glm(sr10 ~ pop15 + pop75 + dpi,
        data = DF,
        family = binomial)
  x <- Tbll_reg(
    fit,
    names = c("Leistung"),
    include.ci = FALSE,
    include.odds = TRUE,
    include.odds.ci = TRUE,
    include.p = TRUE,
    include.b = TRUE,
    include.se = FALSE
  )

  expect_s3_class(x, "data.frame")

  expect_named(x,
               c("term" ,
                 "b" ,
                 "odds",
                 "odds.conf",
                 "p"))



  rst<- x$b
  names(rst) <- x$term


  expect_equal(
  rst[1:4],
  formatC(coef(fit), digits = 3)
  )
})


test_that("glm und lm gemischt reihenfolge", {

fitlm1 <-  lm(sr  ~   pop15 + pop75 + ddpi,  data = LifeCycleSavings)
fitlm2 <-  lm(sr  ~   pop15 + pop75 + dpi,   data = LifeCycleSavings)
fitglm1 <- glm(sr10 ~ pop15 + pop75 + dpi,   data = DF, family = binomial)
fitglm2 <- glm(sr10 ~ pop75 + dpi +   pop15, data = DF, family = binomial)

x <- Tbll_reg(fitlm1,fitlm2,
              fitglm1,fitglm2,
              include.b = TRUE,
              include.se = FALSE,
              include.beta = FALSE,
              include.ci = FALSE,
              include.odds = FALSE,
              include.odds.ci = FALSE,
              include.statistic = FALSE,
              include.p = FALSE,
              include.stars = FALSE)

expect_equal( x$fitglm1_b , x$fitglm2_b)

})

