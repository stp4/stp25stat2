context("test-Tbll_likert")


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

test_that("Likert", {
  x <-
    Tbll_likert(DF2,
                Magazines,
                Comic.books,
                Fiction,
                Newspapers)


  expect_s3_class(x, "data.frame")

  expect_named(
    attributes(x),
    c(
      "class",
      "row.names",
      "names",
      "caption",
      "note" ,
      "N",
      "labels",
      "plot"
    )
  )

  expect_named(attr(x, "plot"),
               c("item",
                 "formula",
                 "results",
                 "nlevels",
                 "ReferenceZero"))

})


test_that("is same as table", {
  x <-
    Tbll_likert(
      DF2,
      Magazines,
      Comic.books,
      Fiction,
      Newspapers,
      include.order = FALSE,
      include.percent = FALSE,
      include.mean = FALSE
    )


  expect_equal(as.vector(unlist(x[1, -1])),
               as.vector(table(DF2$Magazines)))

})


test_that("gehen die Optionen", {
  x <- Tbll_likert(
    DF2,
    Magazines,
    Comic.books,
    Fiction,
    Newspapers,
    by =  ~ Geschlecht,
    include.order = TRUE,
    include.mean = FALSE,
    include.n = TRUE,
    include.na = TRUE,
    include.percent = FALSE,
    ReferenceZero = 2,
    labels = c("neg", "neutum", "pos")
  )



  expect_named(x  ,
               c(
                 "Geschlecht",
                 "Item",
                 "n",
                 "neg(1:1)",
                 "neutum(2)",
                 "pos(3:5)",
                 "NA"
               ))




})
