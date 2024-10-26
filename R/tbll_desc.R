#' Tbll_desc
#'
#' Simple and intuitive formula and pipe-based framework for performing basic statistical Tabels.
#'
#' @param ...  an prepare_data2
#' @param include.n  Anzahl an gueltigen Werten
#' @param include.nr,include.total,include.multiresponse  weitere param
#' @param include.test,include.normality.tests Test
#' @param include.label Labels ja-nein
#' @param use.duplicated erlaube duplikate der Messwerte
#' @param include.custom eigene Funktion mit (x, by, fun) return kann ein Vector oder eine Matrix sein
#'  function(x , by, ...){
#'  x <- scale(as.numeric(x))
#'  diff(sapply(split(x, by), mean, na.rm=TRUE))})
#'
#'  in Tbll_desc_long(include.custom = function(x){ mean(x)})
#' @param include.value vector oder data.frame in exact der Reihenfolge wie die meassure-variablen.
#' @return data.frame
#' @export
#' @examples
#'
#' n<- 100
#' set.seed(1)
#' DF <- data.frame(
#'   sex= gl(2, n/3, labels = c("male", "female"))[sample.int(n)],
#'   group = gl(2, n/3, labels = c("Control", "Treat"))[sample.int(n)],
#'   age= runif(n, min = 18, max = 73)
#'
#' )
#'
#' DF<- na.omit(DF)
#'
#' set_opt(
#'   median = list(digits = 0, style=2),
#'   prozent = list(style=2, null_percent_sign =  ' . ')
#' )
#'
#'
#' DF |>
#'   Tbll_desc( sex[ratio],
#'              group[freq],
#'              age[median],
#'              by= ~group,
#'              include.total=TRUE)
#'
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
#' \donttest{
#' warpbreaks2 <- stp25tools::Label(warpbreaks,
#'                      breaks	=	"The number of breaks",
#'                      wool	=	"The type of wool",
#'                      tension	=	"The level of tension")
#' warpbreaks2$tension2 <- as.numeric(warpbreaks2$tension)
#'
#' warpbreaks2 |>
#'   Tbll_desc(breaks + tension ~ wool)
#' warpbreaks2 |>
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
#'
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
#'   include.se = FALSE
#' )
#'
#'
#' Tbll_reg(lm1, lm2)
#'
#'  }
#'
Tbll_desc <-
  function (...,
            include.label = TRUE,
            include.n = TRUE,
            include.nr = FALSE,
            include.total = FALSE,
            include.test = FALSE,
            include.normality.tests = FALSE,
            include.multiresponse = FALSE,
            include.custom = NULL,
            include.value = NULL,
            digits = NULL,
            use.level = 1, # multiresponse,
            use.duplicated = FALSE
  ) {
    rslt_all <- NULL
    tbl_rstl <- NULL
    caption <- "Summary"
    # Einstellungen
    #
    X <- stp25tools::prepare_data2(...)

    if(use.duplicated){
     # erlaube doppelte parameter
      in_vars <- strsplit(as.character(X$formula)[2L], " \\+ ")[[1L]]
      X$measure.vars <- in_vars
      X$measure.class <- X$measure.class[in_vars]
      X$digits <- X$digits[in_vars]
      X$measure <- X$measure[in_vars]
      X$row_name <- X$row_name[in_vars]
      X$measure.test <- X$measure.test[in_vars]
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


    n <- length(X$measure.vars)
    note <-  "" # measure_info(X$measure)


    any_missing_measure <-
      sapply(X$data[X$measure.vars],
             function(x) length(na.omit(x)))
    if (!include.label)
      X$row_name <- X$measure.vars


    if (include.n & sum(any_missing_measure[X$measure!="header"] - X$N) == 0) {
      # keine fehlenden dann nur erste Zeile mit N
      include.n <- FALSE
      include.nr <- TRUE
    }
    if (include.multiresponse){
      # wegen Formel und weil hier auch Zahlen kommen
      if(!is.null(digits)) X$digits <- rep(0, length(X$digits))
      X$measure <- rep("multi", length(X$measure))
    }

    # Start der Auswertung
    #
    # 1. Mittelweret mit purrr::pmap
    #
    if (is.null(X$group.vars)) {
      include.total <- FALSE
      rslt_all <-
        list_rbind(purrr::pmap(
          list(
            x = X$data[X$measure.vars],
            digits = X$digits,
            measure = X$measure,
            row_name = X$row_name,
            use.level = use.level
          ),
          prct_or_mean
        ))
    }
    #
    # 2. oder include.total  mit purrr::pmap
    #
    if (include.total) {
      rslt_all <-
        list_rbind(purrr::pmap(
          list(
            x = X$data[X$measure.vars],
            digits = X$digits,
            measure = X$measure,
            row_name = X$row_name,
            use.level = use.level
          ),
          prct_or_mean
        ))
      names(rslt_all)[3:ncol(rslt_all)] <-
        paste0("Total_", names(rslt_all)[3:ncol(rslt_all)])
    }
    #
    # 3. Gruppenvergleich mit split() und for()
    #
    if (!is.null(X$group.vars)) {
      if(sum(any_missing_measure)>0 & is.null(get_opt("prozent", "exclude")))
        warning("Achtung exclude geht nur mit addNA()",  call. = FALSE, immediate. = TRUE)

      if (length(X$group.vars) > 1) {
        X$data$group <- interaction2(X$data[X$group.vars])
        caption <- paste(X$group.vars, collapse = ", ")
        X$group.vars <- "group"
      } else {
        caption <-  X$group.vars
      }

      data <- split(X$data[X$measure.vars], X$data[[X$group.vars]])
      if(any(sapply(data, lengths) == 0 )) {
        cat("\n\nMoegliche Fehlerquelle:\nIn der Gruppen-variable gibt es Leere Factoren!\n\n")
        print(table( X$data[[X$group.vars]], useNA ="ifany"))
      }


      for (i in names(data)) {
        tbl_part_i <-
          list_rbind(purrr::pmap(
            list(
              x = data[[i]],
              digits = X$digits,
              measure = X$measure,
              row_name = X$row_name,
              use.level = use.level
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
    #
    # 4. Anzahl entweder als Spalte oder als Singel-Zeile
    #
    if (include.nr) {
      n.out <- c("(N)", rep("", ncol(rslt_all) - 1))
      names(n.out) <- names(rslt_all)

      if (is.null(X$group.vars)) {
        n.out[names(rslt_all) == "m" ] <- X$N
      }
      else {
        tsum <- table(X$data[[X$group.vars]])
        if (include.total) {
          n.out[stringr::str_ends(names(rslt_all), "_m")] <-
            c(as.character(sum(tsum)),
              as.character(tsum))
        }
        else{
          n.out[stringr::str_ends(names(rslt_all), "_m")] <-
            as.character(tsum)
        }
      }
      rslt_all <- rbind(n.out, rslt_all)
    }
    if (!include.n) {
      length.out <- if (is.null(X$group.vars)) 1
                    else nlevels(X$data[[X$group.vars]]) + include.total
      rslt_all <-
        rslt_all[-(seq(
          from = 3,
          by = 2,
          length.out = length.out
        ))]
      names(rslt_all) <- gsub("_m$", "", names(rslt_all))
    }
    #
    # workaround um eindeitige row-names zu bekommen
    # die werden zum mergen gebraucht
    rownames(rslt_all) <- gsub("\\.first_factor", "", rownames(rslt_all))
    #
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
    #
    # Signifikanz Test
    #
    if (include.test) {
      note<- paste(note, ". Test Statistic:", sep = "")
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
      note <-paste(note, " ",
                   paste(unique(names(rslt_test)[nzchar(names(rslt_test))]),
                         collapse = ", "), ".", sep = "")
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
    #
    # Rechts extra Spalten einfuegen
    #
    if (!is.null(include.value)) {
      if (is.vector(include.value) & !is.null(names(include.value))) {
        rslt_value <- rep("", nrow(rslt_all))
        for (i in names(include.value)) {
          pos <- which(rownames(rslt_all) == i)
          rslt_value[pos] <- include.value[i]
        }
        rslt_all$value <- rslt_value
      }
      else if (is.vector(include.value)) {
        stop("Fehler in include.value, jetzt sind zwingend namen bei 'vector' erforderlich!")
      }
      else if (is.data.frame(include.value)) {
        lng_rslt_all <- nrow(rslt_all)

        if (any(rownames(rslt_all) %in% rownames(include.value)))
          rslt_all <-
            stp25tools::Merge2(rslt_all,
                               include.value,
                               by = 0,
                               all = TRUE,
                               sort = FALSE,
                               suffixes = c("", ".1"),
                               include.label = FALSE)
        else{
          stop("Fehler in include.value, jetzt sind zwingend rownames bei 'data.frames' erforderlich!")
        }
        if (lng_rslt_all != nrow(rslt_all)){
          warning("Fehler in include.value, eventuell stimmen die rownames nicht")
        }
      }
      else if (is.matrix(include.value)) {
        if (any(rownames(rslt_all) %in% rownames(include.value)))
          rslt_all <-
            stp25tools::Merge2(rslt_all,
                               data.frame(include.value),
                               by = 0,
                               all = TRUE,
                               sort = FALSE,
                               suffixes = c("", ".1"),
                               include.label = FALSE)
        else{
          stop("Fehler in include.value, jetzt sind zwingend rownames bei 'matrix' erforderlich!")
        }
      }
      else{
        stop("Fehler in include.value, mit dem Daten-Type ",
             class(include.value),
             " kann ich nichts anfangen!")
      }
    }

    rslt_all[[1]] <- paste(rslt_all[[1]], rslt_all[[2]])
    prepare_output(
      names_option(rslt_all[-2]),
      caption = caption,
      note = note,
      N = X$N
    )

  }


names_option <- function(rslt_all) {
  names(rslt_all)[1] <- get_opt("table", "stubhead")
  if (names(rslt_all)[2] == "m")
    names(rslt_all)[2] <- get_opt("table", "measure.name.m")
  else  if (names(rslt_all)[2] == "Total")
    names(rslt_all)[2] <- get_opt("table", "measure.name.total")

  if (names(rslt_all)[ncol(rslt_all)] == "statistics")
    names(rslt_all)[ncol(rslt_all)] <-
      get_opt("table", "measure.name.statistics")
  rslt_all

}


#' @rdname Tbll_desc
#'
#' @description
#' Tbll_desc_multi: ruft direkt Tbll_desc auf (mit unterschiedlichen Parametern).
#'
#' @param by  Gruppenvariable
#' @param use.level,include.order,exclude.last.order Tbll_desc_multi weitere einstellungen
#' @export
#'
#' @examples
#'
#' dat <- data.frame(
#'   Hotel =  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
#'   Pension = c(1, 1, 0, 0, 0, 1, 1, 1, 1, 1),
#'   Hostel = c(1, 1, 0, 1, 1, 0, 0, 1, 0, 1),
#'   Ferienwohnung = c(1, 1, 1, 1, 0, 0, 0, 1, 0, 0),
#'   Airbnb = c(0, 0, 0, 0, 0, 1, 0, 1, 1, 1),
#'   Campingplatz = c(0, 0, 0, 1, 0, 1, 1, 0, 0, 0),
#'   Couchsurfing =c(0,0,0,0,0, 0, 1, 0, 0, 0)
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
#'
Tbll_desc_multi <-
  function(...,
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
      ord_rslt <- Tbll_desc(
        ...,
        by = by,
        use.level = use.level,
        include.n = include.n,
        include.nr = include.nr,
        include.custom = function(x, by, ..., use_level = use.level) {
          if (is.logical(x))
            x <-  as.numeric(x)
          else if (is.factor(x))
            x <- ifelse(x == levels(x)[use_level], 1, 0)
          else
            x <- 0
          mean(x, na.rm = TRUE)
        },
        include.multiresponse = TRUE

      )

      rslt_order <- as.numeric(ord_rslt$custom)

      if (grepl("^\\(N)", rslt$Item[1])) {
        rslt_order  <-  order(c(1000,  rslt_order[-1]), decreasing = TRUE)
      }
      else
        rslt_order <- order(rslt_order, decreasing = TRUE)

      if (exclude.last.order) {
        rslt_order[length(rslt_order)] <- 0
      }

      rslt <- rslt[rslt_order,]

    }

    rslt
  }



#' @description Kopie von interaction()
#'  die Labels werden anderst sortiert.
#'
#'  i lauft in umgekehrter richtung und past ist auch umgedreht
#'  ansonsten identich mit interaction
#'
#' @param ...	the factors for
#' @param sep	string to construct the new level labels by joining the constituent ones.
#'
#' @examples
#'  \donttest{
#' interaction2(
#' gl(2, 8, labels = c("Z", "X")),
#' gl(2, 8, labels = c( "A","B")),
#' gl(2, 8, labels = c( "a","b"))
#' )
#' }
#' @noRd
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

    for (i in 1L:narg) {
      f <- as.factor(args[[i]])[, drop = FALSE]
      l <- levels(f)
      if1 <- as.integer(f) - 1L

      if (i == 1) {
        ans <- if1
        lvs <- l
      }
      else {
        ans <- ans * length(l) + if1
        lvs <- paste(rep(lvs, each = length(l)),
                     rep(l, length(lvs)),
                     sep = sep)

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
emty_tbll <-
  function() {
    data.frame(
      lev = "",
      n = "",
      m = "",
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
                         exclude = get_opt("prozent", "exclude"),
                         max_factor_length = 35,
                         use.level = 1) {
  if (!(measure %in% c("factor", "logical")) | (!is.null(exclude)))
    x  <- na.omit(x)

  n  <- length(x)
  rslt <- NULL

  res <- switch(
    measure,
    numeric = mean_tbll(x, digits, n),
    integer = mean_tbll(x, digits, n),
    factor =  prct_tbll(x, digits, n, exclude, max_factor_length),
    # logical = prct_tbll(x, digits, n, exclude, max_factor_length),
    logical = multi_tbll(x, digits, n, use.level = TRUE),
    mean =    mean_tbll(x, digits, n),
    median =  median_tbll(x, digits, n),
    multi =   multi_tbll(x, digits, n, use.level = use.level),
    ratio =   ratio_tbll(x, n),
    header =  emty_tbll(),
    emty_tbll()
  )

  if (measure == "factor") {
    x0 <- data.frame(
      Item = row_name,
      lev =  get_opt("prozent", "include_name"),
      n = res$n[1] ,
      m = "",
      stringsAsFactors = FALSE,
      row.names="first_factor"
    )
    res$n <- ""
    x1 <- cbind(Item = sep, res)
    rslt <- rbind(x0, x1)
  } else {
    rslt <-  cbind(Item = c(row_name, rep("", nrow(res) - 1)), res)
  }

  rslt
}


#' @noRd
median_tbll <- function(x,
                        digits = 2,
                        n = length(x),
                        style = get_opt("median","style"),
                        include.level = get_opt("median","include_name")) {
  if (is.null(include.level))
    include.level <- ""

  data.frame(
    lev = include.level,
    n = as.character(n),
    m = calc_median(x, digits, n, style),
    stringsAsFactors = FALSE
  )

}


#' @noRd
mean_tbll <- function(x,
                      digits = get_opt("mean", "digits"),
                      n = length(x),
                      style = get_opt("mean","style"),
                      include.level = get_opt("mean", "include_name")) {
  if (is.null(include.level))
    include.level <- ""

  data.frame(
    lev = include.level,
    n = as.character(n),
    m =   calc_mean(x, digits, n, style),
    stringsAsFactors = FALSE
  )
}


#' @param n intern
#' @param exclude geh noch nicht
#' @param max_factor_length lange Einträge kuerzen
#' @noRd
prct_tbll <-
  function(x,
           digits = get_opt("prozent", "digits"),
           n = length(x),
           exclude = NA,
           # c(NA, NaN),
           max_factor_length = 25,
           style = get_opt("prozent", "style"),
           is_true_false = FALSE)  {
    tbl <-  calc_percent(
      x,
      digits = digits,
      n = n,
      exclude = exclude,
      max_factor_length = max_factor_length,
      style = style,
      is_true_false = is_true_false
    )

    data.frame(
      lev = names(tbl),
      n = c(n, rep("", length(tbl) - 1)),
      m = tbl,
      stringsAsFactors = FALSE,
      row.names = paste("[", stp25tools::abbreviate2(names(tbl)), "]",  sep = "")
    )

  }


#' @param use.level welcher level wir gezaelt
#' @param include.level mit (yes) in labels?
#' @noRd
multi_tbll <- function(x,
                       digits = get_opt("prozent", "digits"),
                       n = length(x),
                       use.level = 1,
                       include.level = get_opt("prozent", "include_level_multi"),
                       style = get_opt("prozent", "style")) {
  if (is.null(include.level)) include.level <- TRUE

  if (is.logical(x)) {
    res <-
      prct_tbll(x,
                digits = digits,
                n = n,
                is_true_false = TRUE)
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

  res$lev <- paste( res$lev , get_opt("prozent", "include_name") )

  res
}


ratio_tbll<- function(x,
                      n = length(x),
                      sep = ":") {
  rslt<- table(x)

  data.frame(
    lev =  paste0("(", paste(names(rslt), collapse = sep), ")"),
    n = n,
    m =  paste(rslt, collapse = sep),
    stringsAsFactors = FALSE
  )

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
#' @param measure  intern c("mean", "median",
#' "numeric", "factor", "logical")
#' @param measure.test, test  intern c("cattest",
#' "contest", "notest", "ttest", ...)
#' @param ... description folgt
#' @examples
#'  \donttest{
#'   effect_size(c(2, 3, 4, 2, 3, 4, 3, 6,
#'   7, 6, 8, 9, 4, 5, 6, 7) ,
#'   gl(2, 8, labels = c("Control", "Treat")))
#'   x<- c(2, 3, 4, 2, 3, 4, 3, 6,7, 6, 8, 9, 4, 5, 6, 7)
#'   y<- factor(c(2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 2, 1, 2))
#'    z<- gl(2, 8, labels = c("Control", "Treat"))
#'    rbind(
#'   effect_size(x, z),
#'   effect_size(y, z)
#'   )
#' }
#'
effect_size <- function(x,
                        by,
                        measure,
                        measure.test,
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
