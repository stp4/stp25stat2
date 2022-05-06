context("test-Tbll_desc")

#require(stp25stat2)
data(infert, package = "datasets")
infert$case  <- factor(infert$case , 1:0, c("case", "control"))
infert$spontaneous <- factor(infert$spontaneous)
infert$induced2    <- factor(infert$induced == 0)



test_that("einfache Auswertung", {



  x1<-Tbll_xtabs( ~  case, infert)
  x2<- Tbll_xtabs( ~ induced2 + case, infert)
  x3 <-Tbll_xtabs( ~ induced + case, infert)
  x4<- Tbll_xtabs( ~ induced + education, infert)

  expect_s3_class(x1$xtab, "data.frame")
  expect_s3_class(x2$xtab, "data.frame")
  expect_s3_class(x3$xtab, "data.frame")
  expect_s3_class(x4$xtab, "data.frame")


  expect_named(x4$xtab,
               c("induced",
                 "education_0-5yrs",
                 "education_6-11yrs",
                 "education_12+ yrs"))

})


test_that("arbeien die margins", {

a <-Tbll_xtabs( ~ induced + education + case,
                infert,
                margin = "case",
                #  add.margins = c("education", "induced"),
                include.percent= FALSE)

b<-Tbll_xtabs( ~ induced + education + case,
               infert,
               margin = 3,
               #  add.margins = c("education", "induced"),
               include.percent = FALSE)


expect_equal(
  as.character(a$xtab$case_control),
  c(
    "4",    "57",    "35",    "96",    ".",    "16",
    "29",    "45",    "4",    "7",    "13",    "24",
    "8",    "80",    "77",    "165"
  )
)

expect_identical(a, b)

})


test_that("include.test", {

  df <- data.frame(
    A = c(1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1),
    B = c(0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0)
  )
  x <-
    Tbll_xtabs(
      ~ A + B,
      df,
      include.percent = FALSE,
      include.test = TRUE,
      include.diagnostic = TRUE
    )


  expect_named(x,
               c("xtab",
                 "fisher_test",
                 "diagnostic.test"))

})


test_that("Helper-Function margin workes",{

  df <-
    data.frame(
      sex = gl(2, 8, labels = c("m", "f")),
      edu = gl(2, 8, labels = c("low", "hig")),
      control = gl(2, 8, labels = c("Control", "Treat"))
    )

  x <- xtabs(~ sex + edu, df)

  expect_equal(
    stp25stat2:::get_margins(x),
    list(add = NULL,prop = NULL)
  )

  expect_equal(
    stp25stat2:::get_margins(x,
                            margin = c("sex")),
    stp25stat2:::get_margins(x,
                            margin = c("sex"),
                            add.margins = NA)
  )

  expect_equal(
    stp25stat2:::get_margins(x,
                            margin = c("sex"),
                            add.margins = NULL),
    list(add = NULL, prop = 1)
  )

  expect_equal(
    stp25stat2:::get_margins(x,
                            add.margins = c("sex", "edu")),
    stp25stat2:::get_margins(x,
                            include.total = TRUE)
  )

  expect_equal(
    stp25stat2:::get_margins(x,
                            margin = c("sex")),
    stp25stat2:::get_margins(x,
                            include.total.columns =  TRUE)
  )

  expect_equal(
    stp25stat2:::get_margins(x,
                            margin = c("edu")),
    stp25stat2:::get_margins(x,
                            include.total.rows =  TRUE)
  )

  x2 <- xtabs(~ sex + edu + control, df)

  expect_equal(
    stp25stat2:::get_margins(x2,
                            margin = c("sex", "control")),
    stp25stat2:::get_margins(x2,
                            include.total.sub = TRUE)
  )

})


test_that("Helper-Function fun klassifikation works", {

  data(hkarz, package = "stp25data")
  fit1 <- glm(gruppe ~ lai, hkarz, family = binomial)

  x <- Klassifikation(fit1)
  expect_equal(x$statistic[c(1, 7, 8), 2],
               c("0.82" , "0.81", "0.84"))

  rst <- pROC::auc(pROC::roc(x$response, x$predictor))
  expect_equal(round(rst, 3), 0.818)
})
