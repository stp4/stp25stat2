#'  Tabelle PCA
#'
#'  Bortz Schuster 2010 Seite 396
#'
#'  Stichprobengroesse: Wenn je Faktor zehn oder mehr Variablken anfallen,
#'  ist ein n von 150 ausreichend.
#'
#'  Mindestens vier Variablen muessen Ladungen > 0.60 aufweisen, oder 10 bis 12
#'  um 0.40 dann ist eine generalisierende Interpretation zulässig.
#'
#'  Faktorladung ~ Korelation zwichen Variable und Faktor
#'
#'  Kommunalität ~ Varianz die durch die Faktoren aufgeklaert wird
#'
#'  Eigenwert ~ Gesamt-Varianz aller Variablen bezogen auf die Faktoren.
#'
#'
#'
#' @param x psych::principal
#' @param cut display loadings
#' @param sort by value
#' @param digits  digits
#' @param ... not used
#'
#' @return data.frame or list with data.frame
#' @export
#'
#' @examples
#'
#' require(magrittr)
#' library(stp25data)
#'
#'
#' rslt <- psych::principal(fkv, 5)
#'
#' rslt  %>%  Tbll_pca_loadings()
#' rslt  %>%  Tbll_pca_eigen()
#' rslt  %>%  Tbll_pca_test()
#'
Tbll_pca <- function(x,
                    cut = .30,
                    sort = TRUE,
                    digits = 2,
                    ...
                    ){

  rslt <- extract_principal(x,
                            cut = .30,
                            sort = TRUE,
                            digits = 2)


  rslt$eigen <- Tbll_pca_eigen(extprn=rslt)
  rslt
}




#' @rdname Tbll_pca
#' @export
Tbll_pca_loadings <-
  function(x,
           cut = .30,
           sort = TRUE,
           digits = 2,
           ...
  ) {

    if(!is.null(x))
      extract_principal(
        x,
        digits = digits,
        cut = cut,
        sort = sort
      )$loadings
    else extprn$loadings

  }


#' @rdname Tbll_pca
#' @export
Tbll_pca_eigen <- function(x,
                           digits = 2,
                           extprn =NULL,
                           ...
                           ) {
  if(is.null(extprn))
    rslt <- extract_principal(x,  digits = digits)$eigen
  else   rslt <- extprn$eigen

  caption <- attr(rslt, "caption")

  prepare_output(cbind(
    data.frame(Source = rownames(rslt)),
    render_f(rslt, digits = digits)
  ),
  caption = caption)
}


#' @rdname Tbll_pca
#' @export
Tbll_pca_test <- function(x,
                          ...
                          ) {
  rslt <- extract_principal(x)$test
  rslt
}


# Werte aus psyche:pca extrahieren
#' @noRd
extract_principal <-
  function (x,
            digits = 2,
            all = FALSE,
            cut = .30,
            sort = TRUE,
            suppress.warnings = TRUE,
            ...)  {
    res <- NULL
    colnames(x$loadings) <-
      paste0("PC", 1:(length(colnames(x$loadings))))

    if (!is.matrix(x) && !is.null(x$fa) && is.list(x$fa))
      x <- x$fa
    if (!is.null(x$fn)) {
      if (x$fn == "principal") {
        caption <- "Principal Components Analysis (PCA)"
      }
      else {
        caption <- paste("Factor Analysis using method ", x$fm)
      }
    }
    load <- x$loadings
    if (is.null(cut))
      cut <- 0
    nitems <- dim(load)[1]
    nfactors <- dim(load)[2]
    if (sum(x$uniqueness) + sum(x$communality) > nitems) {
      covar <- TRUE
    } else {
      covar <- FALSE
    }

    loads <- data.frame(item = seq(1:nitems),
                        cluster = rep(0, nitems),
                        unclass(load))
    u2.order <- 1:nitems

    if (sort) {
      loads$cluster <- apply(abs(load), 1, which.max)
      ord <- sort(loads$cluster, index.return = TRUE)
      loads[1:nitems,] <- loads[ord$ix,]
      rownames(loads)[1:nitems] <- rownames(loads)[ord$ix]
      items <- table(loads$cluster)
      first <- 1
      item <- loads$item
      for (i in 1:length(items)) {
        if (items[i] > 0) {
          last <- first + items[i] - 1
          ord <- sort(abs(loads[first:last, i + 2]),
                      decreasing = TRUE,
                      index.return = TRUE)
          u2.order[first:last] <- item[ord$ix + first - 1]
          loads[first:last, 3:(nfactors + 2)] <-
            load[item[ord$ix + first - 1],]
          loads[first:last, 1] <- item[ord$ix + first - 1]
          rownames(loads)[first:last] <-
            rownames(loads)[ord$ix + first - 1]
          first <- first + items[i]
        }
      }
    }

    if (max(abs(load) > 1) && !covar)
      note <- "Warning: A Heywood case was detected"

    ncol <- dim(loads)[2] - 2
    rloads <- round(loads, digits)
    fx <- format(rloads, digits = digits)
    nc <- nchar(fx[1, 3], type = "c")
    fx.1 <- fx[, 1, drop = FALSE]
    fx.2 <- fx[, 3:(2 + ncol)]
    load.2 <- as.matrix(loads[, 3:(ncol + 2)])
    fx.2[abs(load.2) < cut] <- paste(rep(" ", nc), collapse = "")
    if (sort) {
      fx <- data.frame(V = fx.1, fx.2)
      if (dim(fx)[2] < 3)
        colnames(fx) <- c("V", colnames(x$loadings))
    }
    else {
      fx <- data.frame(fx.2)
      colnames(fx) <- colnames(x$loadings)
    }

    if (nfactors > 1) {
      if (is.null(x$Phi)) {
        h2 <- rowSums(load.2 ^ 2)
      }
      else {
        h2 <- diag(load.2 %*% x$Phi %*% t(load.2))
      }
    }
    else {
      h2 <- load.2 ^ 2
    }

    if (!is.null(x$uniquenesses)) {
      u2 <- x$uniquenesses[u2.order]
    }
    else {
      u2 <- (1 - h2)
    }

    vtotal <- sum(h2 + u2)

    if (isTRUE(all.equal(vtotal, nitems))) {
      # cat("Standardized loadings (pattern matrix) based upon correlation matrix\n")
      com <- x$complexity[u2.order]

      if (!is.null(com)) {
        res$loadings <- cbind_pca(
          Items = rownames(fx),
          fx,
          h2 = render_f(h2, 2),
          u2 = render_f(u2, 2),
          com = render_f(com, 2),
          caption = "",
          note = ""
        )
      }
      else {
        res$loadings <-
          cbind_pca(
            Items = rownames(fx),
            fx,
            h2 = render_f(h2, 2),
            u2 = render_f(u2, 2),
            caption = "",
            note = ""
          )
      }
    }
    else {
      res$loadings <- cbind_pca(
        Items = rownames(fx),
        fx,
        h2 = render_f(h2, 2),
        u2 = render_f(u2, 2),
        #H2 = render_f(h2/(h2 + u2),2),
        # U2 = render_f(u2/(h2 + u2),2),
        caption = "Unstandardized loadings (pattern matrix) based upon covariance matrix",
        note = ""
      )
    }

    if (is.null(x$Phi)) {
      if (nfactors > 1) {
        vx <- colSums(load.2 ^ 2)
      }
      else {
        vx <- sum(load.2 ^ 2)
      }
    }
    else {
      vx <- diag(x$Phi %*% t(load) %*% load)
    }
    names(vx) <- colnames(x$loadings)
    varex <- rbind(`SS loadings` = vx)
    varex <- rbind(varex, `Proportion Var` = vx / vtotal)
    if (nfactors > 1) {
      varex <- rbind(varex, `Cumulative Var` = cumsum(vx / vtotal))
      varex <- rbind(varex, `Proportion Explained` = vx / sum(vx))
      varex <-
        rbind(varex, `Cumulative Proportion` = cumsum(vx / sum(vx)))
    }

    res$eigen <- cbind_eigen(varex)

    if (!isTRUE(all.equal(vtotal, nitems))) {
      # cat("\n Standardized loadings (pattern matrix)\n")
      fx <- format(loads, digits = digits)
      nc <- nchar(fx[1, 3], type = "c")
      fx.1 <- fx[, 1, drop = FALSE]
      fx.2 <- round(loads[, 3:(2 + ncol)] / sqrt(h2 + u2), digits)
      load.2 <- loads[, 3:(ncol + 2)] / sqrt(h2 + u2)
      fx.2[abs(load.2) < cut] <- paste(rep(" ", nc), collapse = "")
      fx <- data.frame(V = fx.1, fx.2)
      if (dim(fx)[2] < 3)
        colnames(fx) <- c("V", colnames(x$loadings))
      if (nfactors > 1) {
        h2 <- h2 / (h2 + u2)
      }
      else {
        h2 <- h2 / (h2 + u2)
      }
      u2 <- (1 - h2)
      res$stdloadings <- cbind_pca(
        Items = rownames(fx),
        fx,
        h2 = render_f(h2, 2),
        u2 = render_f(u2, 2),
        caption = "Standardized loadings (pattern matrix)"
      )
      if (is.null(x$Phi)) {
        if (nfactors > 1) {
          vx <- colSums(load.2 ^ 2)
        }
        else {
          vx <- diag(t(load) %*% load)
          vx <- vx * nitems / vtotal
        }
      }
      else {
        vx <- diag(x$Phi %*% t(load) %*% load)
        vx <- vx * nitems / vtotal
      }
      names(vx) <- colnames(x$loadings)
      varex <- rbind(`SS loadings` = vx)
      varex <- rbind(varex, `Proportion Var` = vx / nitems)
      if (nfactors > 1) {
        varex <- rbind(varex, `Cumulative Var` = cumsum(vx / nitems))
        varex <-
          rbind(varex, `Cum. factor Var` = cumsum(vx / sum(vx)))
      }
      res$eigen <- cbind_eigen(varex)
    }
    res$test <- prepare_output(
      data.frame(
        Measures = c("n.obs", "Mean item complexity", "RMSR",
                     "empirical chi square"),
        Statistics = c(
          x$n.obs,
          round(mean(x$complexity), 1),
          render_f(x$rms, 2),
          rndr_X(x$chi, NULL, NULL, x$EPVAL)
        ),
        stringsAsFactors = FALSE
      ),
      caption = paste(
        "Test of the hypothesis that ",
        nfactors,
        if (nfactors == 1)
          " component is"
        else
          " components are",
        " sufficient."
      ),
      N = x$n.obs
    )

    res
  }


# Eigenwert

#' @noRd
cbind_eigen <- function(x,
                        digits = 3)  {
  prepare_output(round(x, digits),
                 caption =  "Erklaerte Gesamtvarianz (Eigenwerte)")
}

# data.frame der Ladungen

#' @noRd
cbind_pca <- function(Items,
                      fx,
                      h2,
                      u2,
                      com,
                      caption = "",
                      note = ""
                      ) {
  names(fx)[1] <- "Nr"

  prepare_output(
    cbind(Item = Items, fx, h2,
          stringsAsFactors = FALSE),
    caption = caption,
    note = note
  )
}
