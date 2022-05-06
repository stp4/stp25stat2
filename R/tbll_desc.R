#' Tbll_desc
#'
#' @param ...  an prepare_data2
#' @param caption besser mit Output verwenden
#' @param include.n,include.nr,include.total,include.multiresponse  weitere param
#' @param include.test,include.normality.tests Test
#' @param include.label Labels ja-nein
#' @param include.custom eigene Funktion mit (x, by, fun) return kann ein Vector oder eine Matrix sein
#'  function(x , by, ...){
#' x <- scale(as.numeric(x))
#' diff(sapply(split(x, by), mean, na.rm=TRUE))})
#' @param include.value vector oder data.frame in exact der reihenfolge wie die meassure-variablen.
#' @param include.single.value M und SD in separaten Spalten
#' @return data.frame
#' @export
#' @examples
#'
#' require(magrittr)
#' #require(stp25tools)
#'
#' Tbll_desc(
#'   warpbreaks,
#'   "H1",
#'   breaks,
#'   tension,
#'   by = ~ wool,
#'   include.total = TRUE,
#'   include.n = FALSE,
#'   include.test = TRUE
#' )
#'
#'
#' warpbreaks2 <- stp25tools::Label(warpbreaks,
#'                      breaks	=	"The number of breaks",
#'                      wool	=	"The type of wool",
#'                      tension	=	"The level of tension")
#' warpbreaks2$tension2 <- as.numeric(warpbreaks2$tension)
#'
#' warpbreaks2 %>%
#'   Tbll_desc(breaks + tension ~ wool)
#' warpbreaks2 %>%
#'   Tbll_desc_long(breaks + tension ~ wool)
#'
#'
#'
#' Tbll_desc(
#'   warpbreaks,
#'   # "H1",
#'   breaks,
#'   tension,
#'   by = ~ wool,
#'   #  include.total = TRUE,
#'   # include.n = FALSE,
#'   include.test = TRUE,
#'   include.value = c(breaks = "ES = 26", tension = "OR = .0256")
#'
#' )
#'
#' x <- Tbll_desc(
#'   warpbreaks,
#'   # "H1",
#'   breaks,
#'   tension,
#'   by = ~ wool,
#'   include.total = TRUE,
#'   # include.n = FALSE,
#'   include.test = TRUE,
#'   include.value = data.frame(ES = 1:2, OR = 3:4)
#'
#' )
#' Tbll_corr(breaks ~ tension2, warpbreaks2,  groups = ~ wool)
#'
#' Tbll_xtabs( ~ tension + wool, warpbreaks2, include.label = FALSE)
#'
#' lm1 <- lm(breaks ~ wool + tension, data = warpbreaks2)
#' lm2 <- lm(breaks ~ wool * tension, data = warpbreaks2)
#'
#'
#' Tbll_reg(
#'   lm1,
#'   lm2,
#'   include.p = FALSE,
#'   include.ci = TRUE,
#'   include.se = FALSE,
#'   caption = "Regression Table"
#' )
#'
#'
#' Tbll_reg(lm1, lm2)

Tbll_desc <- function (...,

                       include.label=TRUE,
                       include.n = TRUE,
                       include.nr = FALSE,
                       include.total = FALSE,
                       include.test = FALSE,
                       include.normality.tests=FALSE,
                       include.multiresponse=FALSE,
                       include.custom = NULL,
                       include.value = NULL,
                     #  include.ci=FALSE,
                     ## #' @param exclude,exclude.level,max_factor_length fuer factor
                       include.single.value =TRUE,
                       digits=NULL,
                       use.level=1 # multiresponse
                       ) {
  caption<- "Summary"
  note<-""
  rslt_all <- NULL
  tbl_rstl <- NULL


  X <- stp25tools::prepare_data2(...)
  n <- length(X$measure.vars)


  any_missing_measure <-
    sapply(X$data[X$measure.vars],
           function(x) length(na.omit(x)))

 if(!include.label)
     X$row_name <- X$measure.vars

  if ( include.n & sum(any_missing_measure[X$measure!="header"]-X$N) == 0 ) {
    # keine fehlenden dann nur erste Zeile mit N
    include.n <- FALSE
    include.nr <- TRUE
  }


  if (include.multiresponse){
   # wegen Formel und weil hie auch Zahlen kommen
     if(!is.null(digits)) X$digits <- rep(0, length(X$digits))
      X$measure <- rep("multi", length(X$measure))
      }

  # start der eigendlichen funktion
  # 1. Mittelweret mit purrr::pmap
  #
  if (is.null(X$group.vars) | include.total) {
    rslt_all <-
      list_rbind(purrr::pmap(
        list(
          x = X$data[X$measure.vars],
          digits = X$digits,
          measure = X$measure,
          row_name = X$row_name,
          use.level = use.level,
          include.single.value = include.single.value
        ),
        prct_or_mean
      ))

    if (include.total)
      names(rslt_all)[3:ncol(rslt_all)] <-
        paste0("Total_", names(rslt_all)[3:ncol(rslt_all)])
  }
  #
  # 2. Gruppenvergleich mit split() und For()
  #
  if (!is.null(X$group.vars)) {
    if (length(X$group.vars) > 1) {
      X$data$group <- interaction2(X$data[X$group.vars])
      caption <- paste(X$group.vars, collapse = ", ")
      X$group.vars <- "group"
    } else {
      caption <-  X$group.vars
    }

    data <- split(X$data[X$measure.vars], X$data[[X$group.vars]])

    for (i in names(data)) {
      tbl_part_i <-
        list_rbind(purrr::pmap(
          list(
            x = data[[i]],
            digits = X$digits,
            measure = X$measure,
            row_name = X$row_name,
            use.level = use.level,
            include.single.value = include.single.value
          ),
          prct_or_mean
        ))


      if (is.null(tbl_rstl))
        tbl_rstl <- tbl_part_i[1:2] # Linke Seite der Tabelle

      names(tbl_part_i) <-  paste0(i, "_", names(tbl_part_i))
      tbl_rstl <- cbind(tbl_rstl, tbl_part_i[-c(1:2)])
    }


    if (include.total)
      rslt_all <- cbind(rslt_all, tbl_rstl[-c(1:2)])
    else
      rslt_all <- tbl_rstl
  }


  if (include.nr) {
    if (is.null(X$group.vars)) {
      n.out <- c("(N)", "", "", X$N)
      names(n.out) <- names(rslt_all)
      rslt_all <- rbind(n.out, rslt_all)
    }
    else {
      tsum <- table(X$data[[X$group.vars]])
      if (include.total) {
        n.out <- c("(N)", rep("", ncol(rslt_all) + 4))
        n.out[stringr::str_ends(names(rslt_all), "_m")] <-
          as.character(c(sum(tsum), as.vector(tsum)))
      } else{
        n.out <- c("(N)", rep("", ncol(rslt_all) + 2))
        n.out[stringr::str_ends(names(rslt_all), "_m")] <-
          as.character(tsum)
      }
      rslt_all <- rbind(n.out, rslt_all)
    }
  }


  if (!include.n) {
    length.out <- if (is.null(X$group.vars)) 1 else nlevels(X$data[[X$group.vars]]) + include.total

    if (include.single.value) {
      rslt_all <-
        rslt_all[-(seq(
          from = 3,
          by = 2,
          length.out = length.out
        ))]
      names(rslt_all) <- gsub("_m$", "", names(rslt_all))
    }
    else{
      rslt_all <-
        rslt_all[-(seq(
          from = 3,
          by = 3,
          length.out = length.out
        ))]
    }

  }


  #  Eigene Funktion fun(x, by, measure, measure.test)
  #  return vector oder matrix
  #  die länge ist gleich wie bei measure oder die anzahl an factoren
  #
  if (!is.null(include.custom)) {
    rslt_custom <- NULL
    # schleife statt purrr::pmap weil es einfacher lesbar ist
    for (i in seq_len(n)) {
      if (is.null(X$group.vars))
        tmp <- do.call(
          include.custom, # include.custom ist eine function
          list(
            X$data[[X$measure.vars[i]]],
            measure = X$measure[i],
            measure.test = X$measure.test[i]
          )
        )
      else
        tmp <- do.call(
          include.custom,
          list(
            X$data[[X$measure.vars[i]]],
            X$data[[X$group.vars[1]]],
            measure = X$measure[i],
            measure.test = X$measure.test[i]
          )
        )
      # tmp kann ein vector der laenge 1 oder eine matrix sein
      if (is.vector(tmp)) {

        if (X$measure[i] != "factor") {
          rslt_custom <- append(rslt_custom, tmp)
        }
        else  if (X$measure[i] == "factor" & length(tmp) == 1) {
          rslt_custom <- append(rslt_custom, tmp)
          rslt_custom <-
            append(rslt_custom, rep("", nlevels(X$data[[X$measure.vars[i]]])))
        } else if (X$measure[i] == "factor" &
                   length(tmp) == nlevels(X$data[[X$measure.vars[i]]])) {
          rslt_custom <- append(rslt_custom, c("", tmp))
        } else{
          stop("In rslt_custom stimmen die Laenge der Rueckgabe nicht!")
        }
      }
      else{
        if (X$measure[i] != "factor") {
          rslt_custom <- rbind(rslt_custom, tmp)
        }
        else if (X$measure[i] == "factor" & nrow(tmp) == 1) {
          rslt_custom <- rbind(rslt_custom, tmp)
          rslt_custom <- rbind(rslt_custom,
                               matrix(
                                 "",
                                 ncol = ncol(rslt_custom),
                                 nrow = nlevels(X$data[[X$measure.vars[i]]])
                               ))
        }
        else if (X$measure[i] == "factor" &
                 nrow(tmp) == nlevels(X$data[[X$measure.vars[i]]])) {
          rslt_custom <- rbind(rslt_custom,
                               matrix("",
                                      ncol = ncol(rslt_custom),
                                      nrow = 1))
          rslt_custom <- rbind(rslt_custom, tmp)
        } else{
          stop("In rslt_custom stimmen die Laenge der Rueckgabe nicht!")
        }



      }
    }


    if (is.vector(rslt_custom)) {
      if (include.nr)
        rslt_custom <-  append(rslt_custom, "", after = 0)
      rslt_all$custom <- rslt_custom
    }
    else {
      # is.matrix
      if (include.nr)
        rslt_custom <- rbind(rep("", ncol(rslt_custom)), rslt_custom)
      rslt_all <- cbind(rslt_all, rslt_custom)
    }
  }

  if (is.character(include.test)) {
    include.test <- gsub("[^[:alpha:]]", "", tolower(include.test))
    which_test <-
      match.arg(include.test,
                c(contest, cattest, notest, ordtest, disttest, cortest))


    X$measure.test <- rep(which_test, length(X$measure.test))
    if (which_test %in% disttest) {
      include.test <- FALSE
      include.normality.tests <- TRUE
    } else{
      include.test <- TRUE
    }
  }

  if (include.test) {

    rslt_test <- NULL
    for (i in seq_len(n)) {
      temp <- NULL
      fm_chi <-
        formula(paste("~", X$measure.vars[i], "+", X$group.vars[1]))
      fm_aov <-
        formula(paste(X$measure.vars[i], "~", X$group.vars[1]))

      if (X$measure.test[i] == "notest") {
        rslt_test <- append(rslt_test,  "")
      }
      else if (X$measure.test[i] == "contest") {
        if (X$measure.class[i] == "factor") {
          temp <- X$data[[X$measure.vars[i]]]
          X$data[[X$measure.vars[i]]] <-
            as.numeric(X$data[[X$measure.vars[i]]])
        }
        rslt_test <-
          append(rslt_test,  conTest(fm_aov, X$data))
      }
      else if (X$measure.test[i] == "cattest") {
        rslt_test <- append(rslt_test, catTest(fm_chi, X$data))
      }
      else if (X$measure.test[i] %in% contest) {
        if (X$measure.class[i] == "factor") {
          temp <- X$data[[X$measure.vars[i]]]
          X$data[[X$measure.vars[i]]] <-
            as.numeric(X$data[[X$measure.vars[i]]])
        }
        rslt_test <-
          append(rslt_test, conTest(fm_aov, X$data, X$measure.test[i]))
      }
      else if (X$measure.test[i] %in% cattest) {
        rslt_test <-
          append(rslt_test, catTest(fm_chi, X$data, X$measure.test[i]))
      }
      if (!is.null(temp))
        X$data[[X$measure.vars[i]]] <- temp
      if (X$measure[i] == "factor")
        rslt_test <-
        append(rslt_test, rep("", nlevels(X$data[[X$measure.vars[i]]])))
    }
    if(include.nr)   rslt_test <-  append(rslt_test, "", after = 0)
    note <- paste(unique(names(rslt_test)[nzchar(names(rslt_test))]), collapse = ", ")
    rslt_all$statistics <- rslt_test
  }

  if (include.normality.tests) {
    rslt_disttest <- NULL
    for (i in seq_len(n)) {
      if (X$measure[i] %in% c("numeric", "mean", "median")) {
        ix <- na.omit(as.numeric(X$data[[X$measure.vars[i]]]))
        if (X$measure.test[i] == "kstest")
          r <- APA(stats::ks.test(ix, "pnorm", mean(ix), sd(ix)))
        else
          r <- APA(stats::shapiro.test(ix))
      } else if (X$measure[i] == "factor")
        r <- rep("", nlevels(X$data[[X$measure.vars[i]]]))
      else
        r <- ""
      rslt_disttest <- append(rslt_disttest, r)
    }
    if (include.nr)
      rslt_disttest <-  append(rslt_disttest, "", after = 0)

    note<- X$measure.test[1]
    rslt_all$normality.tests <- rslt_disttest
  }


  if (!is.null(include.value)) {

    rslt_custom <- NULL
    if (is.vector(include.value)) {

      for (i in seq_len(n)) {
        # include.value[i] kann ein vector der laenge 1 oder eine matrix sein
        if (X$measure[i] != "factor") {
          rslt_custom <- append(rslt_custom, include.value[i])
        }
        else  if (X$measure[i] == "factor" &
                  length(include.value[i]) == 1) {
          rslt_custom <- append(rslt_custom, include.value[i])
          rslt_custom <-
            append(rslt_custom, rep("", nlevels(X$data[[X$measure.vars[i]]])))
        } else if (X$measure[i] == "factor" &
                   length(include.value[i]) == nlevels(X$data[[X$measure.vars[i]]])) {
          rslt_custom <- append(rslt_custom, c("", include.value[i]))
        } else{
          stop("In rslt_custom stimmen die Laenge der Rueckgabe nicht!")
        }
      }
    }

    else{

      for (i in seq_len(n)) {


        if (X$measure[i] != "factor") {

          rslt_custom <- rbind(rslt_custom, include.value[i,])
        }
        else if (X$measure[i] == "factor" & nrow(include.value[i,]) == 1) {

          rslt_custom <- rbind(rslt_custom, include.value[i,])

          rslt_custom <- rbind(rslt_custom,
                               matrix(
                                 "",
                                 ncol = ncol(rslt_custom),
                                 nrow = nlevels(X$data[[X$measure.vars[i]]]),
                                 dimnames = list(NULL,
                                                  colnames(( include.value ))))
                               )
        }
        else if (X$measure[i] == "factor" &
                 nrow(include.value[i,]) == nlevels(X$data[[X$measure.vars[i]]])) {

          rslt_custom <- rbind(rslt_custom,
                               matrix("",
                                      ncol = ncol(rslt_custom),
                                      nrow = 1,
                                      dimnames = list(NULL,
                                                      colnames(( include.value ))))
          )
          rslt_custom <- rbind(rslt_custom, include.value[i,])
        } else{
          stop("In rslt_custom stimmen die Laenge der Rueckgabe nicht!")
        }
      }


    }



    if (is.vector(rslt_custom)) {
      if (include.nr)
        rslt_custom <-  append(rslt_custom, "", after = 0)
      rslt_all$value <- rslt_custom
    }
    else {
      # is.matrix
      if (include.nr)
        rslt_custom <- rbind(rep("", ncol(rslt_custom)), rslt_custom)
      rslt_all <- cbind(rslt_all, rslt_custom)
    }
  }

  rslt_all[[1]] <- paste(rslt_all[[1]], rslt_all[[2]])


  prepare_output(rslt_all[-2], caption = caption, note=note, N=X$N)
}











#' @param include.n,include.mean,include.median,include.sd,include.range Masszahlen
#' @param include.shapiro,include.ks Normalverteilung Tests
#' @param include.skew,include.kurtosi Eigenschaften
#'
#' @rdname Tbll_desc
#' @export
#' @examples
#'
#' df <- data.frame(
#'   month = rep(1:3, 2),
#'   student = factor(rep(c("Amy", "Bob"), each = 3)),
#'   A = c(9, 7, 6, 8, 6, 9),
#'   B = c(6, 7, 8, 5, 6, 7)
#' )
#'
#' Tbll_desc_item( ~ A + B + student, df)
#'
#'
Tbll_desc_item <- function(...,

                        #   include.label = TRUE,
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


#' @rdname Tbll_desc
#' @export
#'
#' @examples
#'
#'  dat <- data.frame(
#' Hotel =  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
#' Pension = c(1, 1, 0, 0, 0, 1, 1, 1, 1, 1),
#' Hostel = c(1, 1, 0, 1, 1, 0, 0, 1, 0, 1),
#' Ferienwohnung = c(1, 1, 1, 1, 0, 0, 0, 1, 0, 0),
#' Airbnb = c(0, 0, 0, 0, 0, 1, 0, 1, 1, 1),
#' Campingplatz = c(0, 0, 0, 1, 0, 1, 1, 0, 0, 0),
#' Couchsurfing =c(0,0,0,0,0, 0, 1, 0, 0, 0)
#'
#' )
#'
#' Tbll_desc_multi(dat,
#'                  Hotel,
#'                  Pension,
#'                  Hostel,
#'                  Ferienwohnung,
#'                  Airbnb,
#'                  Campingplatz,
#'                  Couchsurfing)


Tbll_desc_multi <- function(...,
                            by = NULL,
                            digits = 0,
                            include.order = TRUE,
                            exclude.last.order = FALSE,

                            use.level = 1,

                            include.label = TRUE,
                            include.n = TRUE,
                            include.nr = FALSE,
                            include.total = FALSE,
                            include.test = FALSE,
                            include.normality.tests = FALSE,

                            include.custom = NULL,
                            include.value = NULL) {


  rslt <- Tbll_desc(
    ...,
    by = by,
    include.label=include.label,
    include.n = include.n,
    include.nr = include.nr,
    include.total = include.total,
    include.test = include.test,
    include.normality.tests=include.normality.tests,

    include.custom = include.custom,
    include.value = include.value,
    include.multiresponse = TRUE,
    digits = digits,
    use.level=use.level
  )

  if (include.order) {
    rslt_order <- Summarise(
      ...,
      fun = function(x) {
        x <- na.omit(x)
        if (is.logical(x))  mean(x)
        else if (is.numeric(x)) mean(x == 1)
        else
          mean(x == levels(x)[use.level])
      }
    )

    rslt_order<-rslt_order$value
    if(grepl("^\\(N)", rslt$Item[1] )){
      rslt_order  <- c( max( rslt_order ) +1, rslt_order)
    }

    if (exclude.last.order) {
      rslt_order[length(rslt_order)] <- 0
    }

    rslt <- rslt[order(rslt_order, decreasing = TRUE), ]
  }

  rslt
}





#' @noRd
#'
#' @description Kopie von  interaction
#'  die Labels werden anderst sortiert.
#'
#'   i lauft in umgekehrter richtung und past ist auch umgedreht ansonsten identich mit interaction
#'
#' @param ...	the factors for
#' @param sep	string to construct the new level labels by joining the constituent ones.
#'
#' @examples
#'
#' interaction2(
#' gl(2, 8, labels = c("Z", "X")),
#' gl(2, 8, labels = c( "A","B")),
#' gl(2, 8, labels = c( "a","b"))
#' )
#'
interaction2 <-
  function (...,
            sep = "_") {
    args <- list(...)
    narg <- length(args)
    if (narg < 1L)
      stop("No factors specified")
    if (narg == 1L && is.list(args[[1L]])) {
      args <- args[[1L]]
      narg <- length(args)
    }
    # orginal  for (i in narg:1L)
    for (i in 1L:narg) {


      f <- as.factor(args[[i]])[, drop = FALSE]
      l <- levels(f)
      if1 <- as.integer(f) - 1L
      # Orginal  if (i == narg) {
      if (i == 1) {
        ans <- if1
        lvs <- l
      }
      else {
        ans <- ans * length(l) + if1
        lvs <- paste(rep(lvs, each = length(l)),
                     rep(l, length(lvs)),
                     sep = sep)
        #orginal paste(rep(l, each = ll), rep(lvs, length(l))
        if (anyDuplicated(lvs)) {
          ulvs <- unique(lvs)
          while ((i <- anyDuplicated(flv <- match(lvs,  ulvs)))) {
            lvs <- lvs[-i]
            ans[ans + 1L == i] <- match(flv[i], flv[1:(i - 1)]) - 1L
            ans[ans + 1L > i] <- ans[ans + 1L > i] - 1L
          }
          lvs <- ulvs
        }
      }
    }
    structure(as.integer(ans + 1L),
              levels = lvs,
              class = "factor")
  }


#' Leerer Data.Frame
#'
#' @noRd
emty_tbll <- function(include.single.value = TRUE) {
  if (include.single.value)
    data.frame(
      lev = "",
      n = "",
      m = "",
      stringsAsFactors = FALSE
    )
  else
    data.frame(
      lev = "",
      n = "",
      m.m = "",
      m.sd = "",
      stringsAsFactors = FALSE
    )
}



#' Die Berechnung
#'
#' @noRd
prct_or_mean <- function(x,
                           digits,
                           measure,
                           row_name,
                           sep = paste(symbol_nbsp(), symbol_nbsp()),
                           exclude = NA,
                           max_factor_length = 35,
                           use.level = 1,
                           include.single.value = TRUE) {
  x  <- na.omit(x)
  n  <- length(x)
  rslt <- NULL

  if (include.single.value) {
    res <- switch(
      measure,
      numeric = mean_tbll(x, digits, n),
      integer = mean_tbll(x, digits, n),
      factor =  prct_tbll(x, digits, n, exclude, max_factor_length),
      logical = prct_tbll(x, digits, n, exclude, max_factor_length),
      mean =    mean_tbll(x, digits, n),
      median =  median_tbll(x, digits, n),
      multi =   multi_tbll(x, digits, n, use.level = use.level),
      header =  emty_tbll(),
      emty_tbll()
    )

    if (measure == "factor") {
      x0 <- data.frame(
        Item = row_name,
        lev = "",
        n = res$n[1] ,
        m = "",
        stringsAsFactors = FALSE
      )
      res$n <- ""
      x1 <- cbind(Item = sep, res)
      rslt <- rbind(x0, x1)
    } else {
      rslt <-  cbind(Item = c(row_name, rep("", nrow(res) - 1)), res)
    }
  } else{
    res <- switch(
      measure,
      numeric = mean_tbll(x, digits, n, style = "two_values"),
      integer = mean_tbll(x, digits, n, style = "two_values"),
      factor =  prct_tbll(x, digits, n, exclude, max_factor_length, style = "two_values"),
      logical = prct_tbll(x, digits, n, exclude, max_factor_length, style = "two_values"),
      mean =    mean_tbll(x, digits, n, style = "two_values"),
      median =  median_tbll(x, digits, n, style = "two_values"),
      multi =   multi_tbll(x, digits, n, use.level = use.level, style = "two_values"),
      header =  emty_tbll(include.single.value = include.single.value),
      emty_tbll(include.single.value = include.single.value)
    )


    if (measure == "factor") {
      x0 <- data.frame(
        Item = row_name,
        lev = "",
        n = res$n[1] ,
        m.m = "",
        m.sd = "",
        stringsAsFactors = FALSE
      )
      res$n <- ""
      x1 <- cbind(Item = sep, res)
      rslt <- rbind(x0, x1)
    } else {
      rslt <-  cbind(Item = c(row_name, rep("", nrow(res) - 1)), res)
    }

    names(rslt) <- c( "Item", "lev", "n", "m", "sd")
  }

  rslt
}


#' @noRd
median_tbll <- function(x,
                           digits = 2,
                           n = length(x),
                           style = get_opt("mittelwert","median.style"),
                           include.level = get_opt("mittelwert","include_name")
) {
  if (is.null(include.level))
    mylevel <- "(median)"
  else if (include.level)
    mylevel <- "(median)"
  else
    mylevel <- ""


  data.frame(
    lev = mylevel,
    n = as.character(n),
    m = calc_median(x, digits, n, style),
    stringsAsFactors = FALSE
  )

}

#' @noRd
mean_tbll <- function(x,
                         digits = 2,
                         n = length(x),
                         style = get_opt("mittelwert","mean.style"),
                         include.level = get_opt("mittelwert", "include_name")
) {
  if (is.null(include.level))   mylevel <- "(mean)"
  else if (include.level)       mylevel <- "(mean)"
  else mylevel <- ""

  data.frame(
    lev = mylevel,
    n = as.character(n),
    m =   calc_mean(x, digits, n, style),
    stringsAsFactors = FALSE
  )
}

#x, digits, n, exclude, max_factor_length

#' @noRd
#' @param n intern
#' @param exclude geh noch nicht
#' @param max_factor_length lange Einträge kuerzen
#'
prct_tbll <-
  function(x,
           digits = 0,
           n = length(x),
           exclude = NA,
           # c(NA, NaN),
           max_factor_length = 25,
           #  return_data_frame = TRUE,

           style = get_opt("prozent", "style"),
           is_true_false = FALSE
           )  {

    if (is.null(style)) style <- 1


    tbl <-  calc_percent(x,
                         digits=digits,
                         n=n,
                         exclude=exclude,
                         max_factor_length=max_factor_length,
                         style=style,
                         is_true_false = is_true_false)


    if (style != "two_values")
      data.frame(
        lev = names(tbl),
        n = c(n, rep("", length(tbl) - 1)),
        m = tbl,
        stringsAsFactors = FALSE
      )
    else
      data.frame(
        lev = rownames(tbl),
        n = c(n, rep("", nrow(tbl) - 1)),
        m = tbl,
        stringsAsFactors = FALSE
      )

  }


#' @noRd
#' @param use.level welcher level wir gezaelt
#' @param include.level mit yes in labels?
multi_tbll <- function(x,
                          digits =0,
                          n = length(x),
                          use.level = 1,
                          include.level = get_opt("prozent", "include_name"),
                          style = get_opt("prozent", "style")) {

 # cat("in multi_tbll\n")
  if (is.null(include.level))
    include.level <- TRUE


  if (is.logical(x)) {
    res <-
      prct_tbll(x,
                           digits = digits,
                           n = n,
                           is_true_false = TRUE,
    )
  } else if (is.factor(x)) {
    res <-
      prct_tbll(
        ifelse(x == levels(x)[use.level], TRUE, FALSE),
        digits = digits,
        n = n,
        is_true_false = TRUE
      )
    res$lev <- levels(x)[use.level]
  } else if (is.numeric(x)) {
    res <-
      prct_tbll(
        ifelse(x ==  use.level, TRUE, FALSE),
        digits = digits,
        n = n,
        is_true_false = TRUE
      )
    res$lev <- use.level
  }
  else
    (stop(class(x)))

  if (!include.level)
    res$lev <- ""


  res
}



#' liste als DF
#'
#' @noRd
list_rbind <- function(l)
  as.data.frame(do.call(rbind, (l)))





#' effect_size
#'
#' Cohen's d  d=(x1-x2)/s psych::cohen.d
#' Hedges' g  g= (x1-x2)/s1s2 ( pooled standard deviation)
#' g enspricht  x<-scale(x)  (mean(x1) - mean(x2))
#'
#' Generalized Log Odds Ratios for Frequency Tables vcd::oddsratio
#'
#' @param x vector
#' @param by factor
#' @param measure  intern c("mean", "median", "numeric", "factor", "logical")
#' @param measure.test, test  intern c("cattest", "contest", "notest", "ttest", ...)
#'
#' @examples
#'
#'  effect_size(c(2, 3, 4, 2, 3, 4, 3, 6,
#' 7, 6, 8, 9, 4, 5, 6, 7) ,
#' gl(2, 8, labels = c("Control", "Treat")))
#' x<- c(2, 3, 4, 2, 3, 4, 3, 6,7, 6, 8, 9, 4, 5, 6, 7)
#' y<- factor(c(2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 2, 1, 2))
#' z<- gl(2, 8, labels = c("Control", "Treat"))
#' rbind(
#'   effect_size(x, z),
#'   effect_size(y, z)
#' )
#'
effect_size <- function(x,
                         by,
                         measure,
                         measure.test,
                       #  test =
                       #    if (measure.test == "contest")
                       #      coin::wilcox_test
                       #  else
                       #    coin::chisq_test)
                         ...) {
  dat <-  na.omit(data.frame(x = x, by = by))
  n <- nrow(dat)
  es <- rslt <-  ""
  if (n > 10) {
    if (measure.test == "contest") {
      es <-   try(psych::cohen.d(as.numeric(dat$x), dat$by), silent = TRUE)
      if (is.character(es)) {
        es <- " error "
      }
      else{
        es <- render_f(es$cohen.d, digits=2)
        es <- paste0(es[2], " [", es[1], ", ", es[3], "] ES")
      }
    }
    else  if (measure.test  == "cattest") {
      # Generalized Log Odds Ratios for Frequency Tables
      if (measure == "factor" & nlevels(dat$x) != 2) {
        es <- "n.a."
      }
      else{
        es <-   try(vcd::oddsratio(table(dat$x, dat$by), log = FALSE),
                    silent = TRUE)
        if (is.character(es)) {
          es <- " error "
        }
        else{
          if (coef(es) < 0.01)
            es <- "n.a."
          else if (coef(es) > 100)
            es <- "n.a."
          else{
            es <- rndr_ods(c(coef(es) ,  confint(es)))
            es <- paste0(es[1], " [", es[2], ", ", es[3], "] OR")
          }
        }

      }
    }


  }
 # cbind('Odds Ratio/Effect Size' = es,
 #       'sig. Test' = rslt)
  cbind('SDM/OR [95% CI]' = es)
}
