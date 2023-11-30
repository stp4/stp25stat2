#' Tbll_likert
#'
#' @param ...  Likert-Objekt oder data+Formula
#' @param caption,note an prepare Data
#' @param ReferenceZero,labels   ReferenceZero=2 Neutrales Element in Kombination mit
#'  labels = c("low", "neutral", "high")
#' @param include.mean,include.n,include.na,include.order,include.percent,include.count,include.total Zusatz

#' @param reverse.levels an Likert
#'
#' @return data.frame mit attributen fuer plotlikert
#' @export
#'
#' @examples
#'
#' #'set.seed(1)
#' n <- 100
#' lvs <- c("--", "-", "o", "+", "++")
#' DF2 <- data.frame(
#'   Magazines = cut(rnorm(n), 5, lvs),
#'   Comic.books = cut(rnorm(n), 5, lvs),
#'   Fiction = cut(rnorm(n), 5, lvs),
#'   Newspapers = cut(rnorm(n), 5, lvs),
#'   Geschlecht = cut(rnorm(n), 2, c("m", "f"))
#' )
#'
#' #x<-Tbll_likert(DF2, Magazines, Comic.books, Fiction, Newspapers, ReferenceZero=2)
#'
#' # stp25plot::likertplot(Item   ~ . , data = x)
#'
#' #
#' Tbll_likert(DF2, Magazines, Comic.books, Fiction, Newspapers,
#'             by=~Geschlecht,
#'             include.order=TRUE)
#'
Tbll_likert <- function(...){
  UseMethod("Tbll_likert")
}


#' @rdname Tbll_likert
#' @param reorder.levels character ore numbers
#' @export
#'
Tbll_likert.default <- function(...,
                                ReferenceZero = NULL,
                                include.mean = TRUE,
                                include.n = FALSE,
                                include.na = FALSE,
                                include.order = TRUE,
                                include.percent = TRUE,
                                include.count = TRUE,
                                include.total=FALSE,
                                decreasing = TRUE,
                                labels = c("low", "neutral", "high"),
                                reverse.levels = FALSE,
                                reorder.levels = NA) {
  rslt <-   Likert(...,
                   reverse.levels = reverse.levels,
                   reorder.levels = reorder.levels,
                   include.total=include.total)

  tbl <-  Tbll_likert.likert(
      rslt,
      ReferenceZero = ReferenceZero,
      include.mean = include.mean,
      include.n = include.n,
      include.na = include.na,
      include.order = include.order,
      include.percent = include.percent,
      include.count = include.count,
      labels = labels,
      decreasing=decreasing
    )



# workaround da
#  HH::likert die sortierung komisch erstellt
  attr(tbl, "plot") <- list(
    item = levels(rslt$results$Item),
    formula =  rslt$formula,
    results =  if(is.null(attr(tbl, "plot")$order)) rslt$results
                  else rslt$results[attr(tbl, "plot")$order,],
    nlevels = rslt$nlevels,
    ReferenceZero = ReferenceZero
  )

 # print( rslt$results )
tbl
}

#' @rdname Tbll_likert
#' @export
#'
Tbll_likert.likert <- function(x,
                               ReferenceZero = NULL,
                               # type = "percent",
                               include.mean = TRUE,
                               include.n = FALSE,
                               include.na = FALSE,
                               include.order = TRUE,
                               #  na.exclude = FALSE,
                               include.percent = TRUE,
                               include.count = TRUE,
                               labels = c("low", "neutral", "high"),
                               decreasing=TRUE) {
  note <- NULL

  if (!is.null(ReferenceZero)) {
    # x$freq und x$freq.na werden neu zudammengefasst
    if (is.character(ReferenceZero))
      ReferenceZero <- which(x$levels %in% ReferenceZero)
    else if (!is.numeric(ReferenceZero))
      ReferenceZero <- median(seq_len(x$nlevels))

    if (ceiling(ReferenceZero) == floor(ReferenceZero)) {
      lowrange <- seq_len((ReferenceZero - 1))
      neutral <- ReferenceZero
      highrange <- (ReferenceZero + 1):x$nlevels

      freq <- cbind(
        lowrange = RowSums2(x$freq[, lowrange]),
        neutral = x$freq[, neutral],
        highrange = RowSums2(x$freq[, highrange])
      )
      if (is.null(note))
        note <-
        paste(
          "lowrange:",
          paste(x$levels[lowrange], "\n", collapse = "|"),
          "neutral:",
          paste(x$levels[neutral], "\n", collapse = "|"),
          "highrange:",
          paste(x$levels[highrange], collapse = "|")
        )

      colnames(freq) <-
        c(
          paste0(labels[1], "(1:", ReferenceZero - 1, ")"),
          paste0(labels[2], "(", ReferenceZero, ")"),
          paste0(labels[3], "(", ReferenceZero + 1, ":", x$nlevels, ")")
        )
      x$freq <- freq

    } else{
      lowrange <- seq_len(floor(ReferenceZero))
      highrange <- ceiling(ReferenceZero):x$nlevels

      freq <-
        cbind(lowrange = RowSums2(x$freq[, lowrange]),
              highrange = RowSums2(x$freq[, highrange]))
      colnames(freq) <-
        c(
          paste0(labels[1], "(1:", floor(ReferenceZero), ")"),
          paste0(labels[3], "(", ceiling(ReferenceZero), ":", x$nlevels, ")")
        )
      if (is.null(note))
        note <-
        paste(
          "lowrange:",
          paste(x$levels[lowrange], "\n", collapse = "|"),
          "highrange:",
          paste(x$levels[highrange], collapse = "|")
        )
      x$freq <- freq
    }

    x$freq.na <- if (names(x$freq.na)[ncol(x$freq.na)] == "NA")
      cbind(freq, x$freq.na[ncol(x$freq.na)])
    else
      freq
  }


  if (include.na) {x$freq <- x$freq.na}

  if (include.percent) {
    if (include.count)  x$freq <- rndr_percent(x$freq / x$n * 100, x$freq)
    else  x$freq <- rndr_percent(x$freq / x$n * 100)
  } else if (!include.count) { x$freq <- "" }

  if (include.n) {x$freq <- cbind(n = x$n, x$freq)}

  if (include.mean) { x$freq <- cbind(x$freq, 'M(SD)' = x$Mittelwert)}

  ans <- cbind(x$names, x$freq)


  attr(ans, "plot") <- list( order= NULL)
  if (include.order) {
    if (length(all.vars(x$formula)) > 2) {
      item.order <- order(x$m + as.numeric(x$results[[1]]) * 100, decreasing=decreasing)
      ans <- ans[item.order,]
      attr(ans, "plot")<- list( order= item.order)
    }
    else{
      item.order<- order(x$m, decreasing=decreasing)
      ans <- ans[item.order,]
      attr(ans, "plot")<- list( order= item.order)
    }
  }


  prepare_output(ans,
                 caption = "Likert",
                 N = x$N)
}


#' @rdname Tbll_likert
#' @export
Likert <- function(...,
                   labels = NULL,
                   reverse.levels = FALSE,
                   reorder.levels = NA,
                   include.total = FALSE
                   ) {
  if (!reverse.levels) {

    if (is.na(reorder.levels)) {

      results <-
        Summarise(...,
          fun = function(x) {
            if(is.logical(x)) x <- factor(x, c(FALSE, TRUE))
            table(x, useNA = "always")
            },
          key = "Item"
      )
      item_mean <-
        Summarise(
          ...,
          fun = function(x)
            mean(as.numeric(x), na.rm = TRUE),
          key = "Item"
        )$value

      item_sd <-
        Summarise(
          ...,
          fun = function(x)
            sd(as.numeric(x), na.rm = TRUE),
          key = "Item"
        )$value
    }
    else {
      results <-
        Summarise(...,
        fun = function(x) {

          if(is.logical(x)) x <- factor(x, c(FALSE, TRUE))

          if (is.numeric(reorder.levels))
            x <- factor(x, levels(x)[reorder.levels])
          else
            x <- factor(x, reorder.levels)

          table(x, useNA = "always")
        },
        key = "Item")

      item_mean <-
        Summarise(
          ...,
          fun = function(x){
            if (is.numeric(reorder.levels))
              x <- factor(x, levels(x)[reorder.levels])
            else
              x <- factor(x, reorder.levels)

            mean(as.numeric(x), na.rm = TRUE)
            },
          key = "Item")$value

      item_sd <-
        Summarise(
          ...,
          fun = function(x){
            if (is.numeric(reorder.levels))
              x <- factor(x, levels(x)[reorder.levels])
            else
              x <- factor(x, reorder.levels)

            sd(as.numeric(x), na.rm = TRUE)
            },
          key = "Item")$value
    }
  } else  {
    results <-
      Summarise(
        ...,
        fun = function(x) {

          if(is.logical(x)) x <- factor(x, c(FALSE, TRUE))
          x <- factor(x, rev(levels(x)))
          table(x, useNA = "always")
        },
        key = "Item")

    item_mean <-
      Summarise(
        ...,
        fun = function(x) {
          mean(1 + nlevels(x) - as.numeric(x), na.rm = TRUE)
        },
        key = "Item"
      )$value

    item_sd <- Summarise(...,
      fun = function(x) {
        sd(1 + nlevels(x) - as.numeric(x), na.rm = TRUE)
      },
      key = "Item" )$value
  }


  # - hier kommt  results, item_mean, item_sd

 # print(results)
 # print(item_mean)
 # print(item_sd)

  nms <-  sapply(results, is.integer)
  ncl <- ncol(results)
  names(results)[ncl] <- "NA"


  col_names <- names(results[-ncl])
  pos_col_names <- grep("Item", col_names)


 str_total <- "Total"
 if(is.character(include.total)) {
  str_total <- include.total
  include.total <- TRUE
 }

  if (include.total) {
    dotts <- stp25tools::prepare_data2(...)
    rslt_total <-
      Likert(
        formula(paste("~", paste(dotts$measure.vars, collapse = "+"))),
        dotts$data)
cat("\nWorkaraund")
    print(results)
    print(rslt_total$results)
    cat("\n")

    rslt_total$results<-  cbind(rslt_total$results[1], rslt_total$freq.na)

    results <- dplyr::bind_rows(results, rslt_total$results )
    results[[1]] <- factor(results[[1]], c(str_total, levels( results[[1]])))
    results[[1]][is.na( results[[1]] )] <- str_total



    item_mean <- c(item_mean, rslt_total$m)
    item_sd <- c(item_sd, rslt_total$sd)

#print(str_total)
    print(results[which(nms)])
    print(rslt_total$freq.na)

  }




  rslt <- list(
    results = results[-ncl],
    names =   results[-c(which(nms), ncl)],
    freq =    results[which(nms[-ncl])],
    freq.na = results[which(nms)],
    N =       sum(results[which(nms)]) / nlevels(results$Item),
    n =       as.vector(rowSums(results[which(nms[-ncl])])),
    m =       item_mean,
    sd =      item_sd,
    Mittelwert = rndr_mean(item_mean, item_sd),
    # items =  data.frame(),#  grouping = NULL,
    formula =  if (pos_col_names == 1) {Item ~ .}
               else{formula(paste("Item ~ .|",
                                  paste(col_names[1:(pos_col_names - 1)], collapse = "+")))},

    nlevels = sum(nms) - 1,
    levels =  names(nms[-ncl])[nms[-ncl]]
  )
  class(rslt) <- c('likert', class(rslt))
  rslt


}

# table_likert <- function(x, useNA = "always"){
#   if(is.logical(x)) x<- factor(x, c(FALSE, TRUE))
#   table(x, useNA = useNA)
#
# }

RowSums2 <- function(x)
  if (is.vector(x)) x else rowSums(x, na.rm = TRUE)


#' @rdname Tbll_likert
#' @export
print.likert<-function(x, ...){
  cat("\nnames: ", paste(names(x), collapse=", "),"\n")
  cat("\nresults:  \n ")
  print( head(x$results))
  cat("\nlevels: ", paste(x$levels, collapse=", "),"\n")
}

