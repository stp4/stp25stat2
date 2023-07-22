#' @rdname extract
#' @description  tbll_extract.meta: Ausgabe von print.meta als tabelle
#' @examples
#'
#'
#'  library(meta)
#' data(Olkin1995)
#' head(Olkin1995)
#' m1 <- metabin(ev.exp, n.exp, ev.cont, n.cont,
#'               data = Olkin1995, subset = c(41, 47, 51, 59),
#'               studlab = paste(author, year),
#'               method = "Inverse")
#' print(m1, digits = 1, digits.Q = 1)
#' Tbll(m1)
tbll_extract.meta <-
  function(x,
           digits = 2,
           include.I2 = TRUE,
           include.Q = TRUE,
           include.tau = TRUE,
           include.H = TRUE,
           ...) {

    # Workaround da die orginale print.meta Funktion keinen export erlaubt
    rslt <-
      capture.output(
        meta:::print.meta(
          x,
          digits = digits,
          digits.pval = 3,
          digits.Q = 1 ,
          digits.pval.Q = 3,
          digits.I2 = 0,
          digits.tau2 = 2,
          digits.H = 2,
          #  digits.tau=2,
          ...
        )
      )

    model <- grepl("Common effect", rslt)
    if (any(model)) {
      model <-  -1:1 + which(model)
      model <-  rslt[model]
      header <-  unlist(strsplit(model[1], " +"))
      dat <- strsplit(model[-1], " +")
      model <- tibble::tibble(
        Model = c("Common effect", "Random effects"),
        Estimate = c(dat[[1]][4],  dat[[2]][4]),
        `95%-CI` = c(paste(dat[[1]][5], dat[[1]][6]),
                     paste(dat[[2]][5], dat[[2]][6])),
        z = c(dat[[1]][7],  dat[[2]][7]),
        `p-value` = c(dat[[1]][8],  dat[[2]][8])
      )
      names(model)[2] <- header[2]
      model <-
        prepare_output(model,
                       caption = "meta-analysis",
                       note =
                             paste(rslt[1], ", method:",
                                   rslt[length(rslt) - 2]))
    } else {
      warnings("Irgendwas ist faul im extract.meta()!")
      return(rslt)
    }


    if (include.I2) {
      i2 <-  grepl("I\\^2", rslt)
      if (any(i2)) {
        i2 <-   which(i2)
        i2 <-  rslt[i2]
        i2 <-  unlist(strsplit(i2, "; H"))[1]
        model <- stp25tools::add_row_df(
          model, trimws(i2), pos = nrow(model) + 1)
      }
    }

    if (include.H) {
      i2 <-  grepl("I\\^2", rslt)
      if (any(i2)) {
        i2 <-   which(i2)
        i2 <-  rslt[i2]
        i2 <-  unlist(strsplit(i2, "; H"))[2]
        model <-
          stp25tools::add_row_df(
            model, paste0("H", i2), pos = nrow(model) + 1)
      }
    }

    if (include.Q) {
      q <-  grepl("Test of heterogeneity", rslt)
      if (any(q)) {
        q <-  2  + which(q)
        q <-  rslt[q]
        q <-  unlist(strsplit(q, " +"))
        q <- paste0("Q(", q[3], ") = ", q[2], ", p = ", q[4])
        model <- stp25tools::add_row_df(model, q, pos = nrow(model) + 1)
      }
    }

    if (include.tau) {
      tau <-  grepl("tau", rslt)
      if (any(tau)) {
        tau <-   which(tau)
        tau <-  rslt[tau]
        tau <-  paste0(unlist(strsplit(tau, "\\]; "))[1], "]")

        model <- stp25tools::add_row_df(
          model, trimws(tau), pos = nrow(model) + 1)
      }
    }

    model

  }
# library(meta)
# data(Olkin1995)
# head(Olkin1995)
# m1 <- metabin(ev.exp, n.exp, ev.cont, n.cont,
#               data = Olkin1995, subset = c(41, 47, 51, 59),
#               studlab = paste(author, year),
#               method = "Inverse")
# print(m1, digits = 1, digits.Q = 1)
# Tbll(m1)
