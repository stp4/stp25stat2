
#' @param include.n,include.mean,include.median,include.sd,include.range Masszahlen
#' @param include.shapiro,include.ks Normalverteilung Tests
#' @param include.skew,include.kurtosi Eigenschaften
#'
#' @rdname Tbll_desc
#' @export
#' @examples
#'
#' require(stp25stat2)
#' require(stp25tools)
#'
#'
#' df <- data.frame(
#'   A = c(1,2,3,4,5, 1,1,1,2, 1,1,1,5,5,5),
#'   B = c(1,2,3,4,5, 2,2,2,1, 1,1,1,5,5,5),
#'   C = c(1,2,3,4,5, 3,3,3,4, 1,1,1,5,5,5),
#'   D = c(1,2,3,5,5, 4,5,4,5, 1,1,1,5,5,5),
#'   E = c(1,2,3,4,5, 3,2,1,1, 1,1,1,5,5,5)
#' )
#'
#' df$index <-scale( rowSums(df) )
#'
#'
#' Tbll_desc_item(  ~ A + B + C + D + E, df)
#'
#' Tbll_reliability(~ A + B + C + D + E, df,
#'                  include.item_statistics = TRUE)
#' Tbll_item_analysis(  A + B + C + D + E ~ index, df)
Tbll_desc_item <- function(...,
                           include.label = FALSE,
                           include.n = TRUE,
                           #  include.missing = TRUE,
                           include.mean = TRUE,
                           include.median = FALSE,
                           include.sd = TRUE,
                           include.range = TRUE,
                           include.shapiro = TRUE,
                           include.ks = FALSE,
                           include.skew = TRUE,
                           include.kurtosi = include.skew,
                           #   na.omit = FALSE,
                           digits = 2) {
  caption<- "Summary"
  note <- ""
  X <- stp25tools::prepare_data2(...)
  if (!include.label)
    X$row_name <- X$measure.vars

  # n_col <- length(X$measure.vars)
  n_row <- length(X$data)
  X$data[X$measure.vars] <-  stp25tools:::dapply1(X$data[X$measure.vars])

  result <- Summarise(
    X$formula,
    X$data,
    fun = function(x) {
      x <- na.omit(as.numeric(x))
      rslt <- c()

      if (include.n)
        rslt <-
        append(rslt,
               c(n = render_f(length(x), 0)), 0)

      if (include.mean)
        rslt <-
        append(rslt,
               c(M = render_f(mean(x), digits)))

      if (include.sd)
        rslt <-
        append(rslt,
               c(SD = render_f(sd(x), digits)))

      if (include.median)
        rslt <-
        append(rslt,
               c(Median = render_f(median(x), digits)))

      if (include.range)
        rslt <-
        append(rslt,
               c(Range = paste0(
                 "[", paste(render_f(range(x), digits), collapse = ", "), "]"
               )))

      if (include.skew | include.kurtosi)
        rslt <-
        append(rslt,
               c(
                 Skew    = render_f(psych::skew(x), 2),
                 Kurtosi = render_f(psych::kurtosi(x), 2)
               ))

      if (include.ks)
        rslt <-
        append(rslt, c(KS.Test = APA(
          stats::ks.test(x, "pnorm", mean(x), sd(x))
        )))

      if (include.shapiro)
        rslt <-
        append(rslt, c(Shapiro.Test =  APA(stats::shapiro.test(x))))

      rslt
    }
  )


  result[[1]] <- as.character(result[[1]])

  is_transformt <- X$measure.vars[which(X$measure.class != "numeric")]
  if (length(is_transformt) > 0) {
    note <-
      paste("Transformed to numeric:",
            paste(is_transformt, collapse = ", "))

  }

  prepare_output(result,
                 caption = caption,
                 note = note,
                 N = n_row)

}




#' @rdname Tbll_reliability
#'
#'
#' @description Tbll_item_analysis: stolen from psychometric::item.exam
#'
#' Sample.SD
#' Standard deviation of the item
#'
#' Item.total
#' Correlation of the item with the total test score
#'
#' Item.Tot.woi
#' Correlation of item with total test score (scored without item)
#'
#' Difficulty
#' Mean of the item (p)
#'
#' Discrimination
#' Discrimination of the item (u-l)/n
#'

#'
#' Item.Reliab
#' Item reliability index
#'
#' Item.Rel.woi
#' Item reliability index (scored without item)
#'
#'
#' Item.Criterion
#' Correlation of the item with the Criterion (y)
#'
#' Item.Validity
#' Item validity index
#'
#' @param ... daten
#' @param include.label Labels
#' @param digits  Nachkommastellen
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#'
#' TestScores <- stp25tools::get_data(
#'   "i1 i2 i3 i4 i5 i6 i7 i8 i9 i10    y
#'   1  1  1  1  1  0  0  1  1   1 2.42
#'   0  0  0  1  1  0  0  0  1   1 0.25
#'   1  0  1  1  1  1  1  1  0   1 6.98
#'   1  1  1  1  1  1  1  0  1   1 6.33
#'   1  0  1  1  1  1  1  1  0   0 6.44
#'   1  1  1  1  1  1  1  1  1   1 3.85
#'   0  0  1  1  1  0  0  0  1   1 4.08
#'   1  1  1  1  1  1  1  1  1   1 6.85
#'   1  1  1  1  1  0  1  1  1   1 3.22
#'   0  0  0  1  1  0  0  0  1   1 1.59
#'   1  1  1  1  1  0  1  1  1   1 3.08
#'   0  0  1  1  1  0  0  0  1   1 5.57
#'   1  1  1  1  1  0  1  1  1   1 3.41
#'   0  0  0  0  1  0  0  0  1   1 4.51
#'   0  0  1  1  1  0  0  0  1   1 1.09
#'   1  0  1  1  1  1  1  1  1   1 6.86
#'   0  0  0  0  1  1  0  0  1   1 2.94
#'   0  0  0  0  0  1  0  0  1   1 2.71
#'   1  1  1  0  0  1  0  0  1   1 2.21
#'   1  0  1  1  1  1  1  1  1   1 4.01
#'   0  0  0  0  0  1  0  0  1   0 2.98
#'   0  0  0  0  0  1  0  0  1   0 2.51
#'   0  0  0  0  0  1  0  0  1   0 1.26
#'   0  0  0  0  0  1  0  0  1   1 1.52
#'   1  1  1  0  0  1  0  0  1   1 6.53
#'   0  0  0  0  0  1  0  0  1   1 4.08
#'   0  0  0  1  1  1  0  0  1   1 2.36
#'   0  0  0  0  0  1  0  0  1   1 3.16
#'   0  0  0  0  0  1  0  0  1   1 5.15
#'   0  0  0  0  1  0  0  0  1   1 0.52")
#' Tbll_item_analysis(TestScores,
#'     i1, i2, i3, i4, i5, i6, i7, i8, i9, i10,
#'     by = ~ y)
#'
Tbll_item_analysis <-
  function (...,
            include.label = FALSE,
            include.Sample.SD =  FALSE,
            include.Item.total = FALSE,   #  (unkorrigierte) Trennschärfe
            include.Alpha = TRUE,
            include.Item.Tot.woi = include.Alpha, # korrigierte Trennschärfe Alpha if Item Deleted
            include.Difficulty = TRUE,    # Itemschwierigkeit
            include.Discrimination = FALSE,

            include.Item.Reliab = TRUE,
            include.Item.Rel.woi = FALSE,

            digits = 2)  {
    X <- stp25tools::prepare_data2(...)
   # n.measure <- length(X$measure.vars)

    if (!include.label)
      X$row_name <- X$measure.vars
    rslt <- data.frame(Items = X$row_name)
    X$data <- stp25tools:::dapply1(X$data)

    x <- X$data[X$measure.vars]

    was_kommt <- apply(x, 2, range)
    if (!(all(range(was_kommt) == 0:1))) {
      warning(
        "Für die Difficulty erwarte ich Werte von 0 bis 1. Die werde werden daher automatisch umcodiert!"
      )
      x <- stp25tools:::dapply1(x, function(z) {
        z <- z - min(z)
        z / max(z)
      })
    }

    x <- as.matrix(x)
    y <-  if( is.null(X$group.vars)) NULL else as.matrix(X$data[X$group.vars])

    k <- ncol(x)
    n <- nrow(x)

    discrim <- discrim(x, k, n)
    TOT <- apply(x, 1, sum)
    TOT.woi <- TOT - (x)

    diff <- apply(x, 2, mean) # Itemschwierigkeit
    rix <- as.vector(cor(x, TOT, use = "complete"))   #  (unkorrigierte) Trennschärfe
    rix.woi <- diag(cor(x, TOT.woi, use = "complete")) # korrigierte Trennschärfe
    sx <- apply(x, 2, sd)
    vx <- ((n - 1) / n) * sx ^ 2
    i.rel <- rix * sqrt(vx)
    i.rel.woi <- rix.woi * sqrt(vx)


    if (include.Difficulty)
      rslt$Difficulty <- render_f(diff, digits = digits)

    if (include.Sample.SD)
      rslt$Sample.SD <- render_f(sx, digits = digits)
    if (include.Item.total)
      rslt$Item.total <- render_f(rix, digits = digits)
    if (include.Item.Tot.woi)
      rslt$Trennschaerfe <- render_f(rix.woi, digits = digits)

    if (include.Discrimination)
      rslt$Discrimination <- render_f(discrim, digits = digits)
    if (include.Item.Reliab)
      rslt$Item.Reliab <- render_f(i.rel, digits = digits)
    if (include.Item.Rel.woi)
      rslt$Item.Rel.woi <- render_f(i.rel.woi, digits = digits)






    if (!is.null(y)) {
      riy <- as.vector(cor(x, y, use = "complete"))
      i.val <- riy * sqrt(vx)
      rslt$Item.Criterion <- render_f(riy, digits = digits)
      rslt$Item.Validity <- render_f(i.val, digits = digits)
    }
    rslt
  }




discrim <- function (x,
                     k = ncol(x),
                     N = nrow(x)) {
  ni <- as.integer(N / 3)
  TOT <- apply(x, 1, mean)
  tmpx <- cbind(x, TOT)[order(TOT),]
  tmpxU <- tmpx[(N + 1 - ni):N,]
  tmpxL <- tmpx[1:ni,]
  Ui <- apply(tmpxU, 2, sum)
  Li <- apply(tmpxL, 2, sum)
  discrim <- (Ui - Li) / ni
  discrim[1:k]
}

# # Examine the items
# #library(psychometric)
# require(stp25stat2)
# require(stp25tools)
# #kennwerte <- item.exam(test_data)
#
# data(TestScores, package = "psychometric")
# # Look at the data
# head(TestScores)
# DF <- as.data.frame(TestScores) |> Label(i1 = "Hallo")
# DF$i1 <- 1:5
# DF$i2 <- 1:5
# Tbll_item_analysis(DF,  i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, by = ~ y)
