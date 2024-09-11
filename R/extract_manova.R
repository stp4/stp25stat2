#' @rdname extract
#'
#' @description MANOVA: extract.manova(x, , test="Wilks") test : "Wilks", "Pillai"
#' @param include.eta die Manova wird ueber heplots::etasq berechnet und die anova mit den SS eta2=SS/SS_total
#' @param test The name of the test statistic c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")
#' @param include.univariate,include.manova logical
#' @param include.order  reorder univariate results by F-values
#'
#' @export
#' @examples
#' \donttest{
#' #- manova ---------------------------------------------
#'
#' ## Set orthogonal contrasts.
#' op <- options(contrasts = c("contr.helmert", "contr.poly"))
#' ## Fake a 2nd response variable
#' npk2 <- within(npk, foo <- rnorm(24))
#' npk2 <- within(npk2, foo2 <- rnorm(24))
#' npk2.aov <- manova(cbind(yield, foo, foo2) ~ block + N * P * K, npk2)
#'
#' stp25stat2:::extract.manova(npk2.aov) #wilks
#' stp25stat2:::extract.manova(npk2.aov, "Pillai")
#'
#' #npk2.aovE <- manova(cbind(yield, foo) ~  N*P*K + Error(block), npk2)
#' # stp25stat2:::extract.manova(npk2.aovE)
#'
#'
#'
#'  DF<- stp25tools::get_data(
#' "C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25data/extdata/manova.sav"
#' )
#'
#' #information from
#' DF$group <- factor(DF$group , 1:3, Cs("website", "nurse ", "video tape" ))
#'
#' z<- as.matrix(DF[,-1])
#' fit1<- manova(z ~ DF$group )
#' stp25stat2:::extract.manova(fit1)
#'
#' summary(fit1)$Eigenvalues
#'
#' # SPSS
#' # Multivariate Tests of Significance (S = 2, M = 0, N = 13 )
#' #
#' # Test Name       Value  Approx. F Hypoth. DF   Error DF  Sig. of F
#' #
#' # Pillais          .48    3.02       6.00      58.00       .012
#' # Hotellings       .90    4.03       6.00      54.00       .002
#' # Wilks            .53    3.53       6.00      56.00       .005
#' # Roys             .47
#' # Note.. F statistic for WILKS' Lambda is exact.
#' # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' # Univariate F-tests with (2,30) D. F.
#' #
#' # Variable   Hypoth. SS   Error SS Hypoth. MS   Error MS          F  Sig. of F
#' #
#' # USEFUL       52.92424  293.96544   26.46212    9.79885    2.70053       .083
#' # DIFFICUL      3.97515  126.28728    1.98758    4.20958     .47216       .628
#' # IMPORTAN     81.82969  426.37090   40.91485   14.21236    2.87882       .072
#' #
#' # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' #   Eigenvalues and Canonical Correlations
#' #
#' # Root No.    Eigenvalue        Pct.   Cum. Pct.  Canon Cor.
#' #
#' # 1          .892      99.416      99.416        .687
#' # 2          .005        .584     100.000        .072
#' #
#' # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'
#'
#' }
#'
#'
#'
extract.manova <-
  function(x,
           test = "Pillai",
           include.manova = !is.null(test),
           include.univariate = TRUE,
           include.eta = FALSE,
           include.order =TRUE,
           ...) {
    muvrt_rslt <- NULL
    unvrt_rslt <- NULL
    rhs <- x$terms[[3L]]

    if (include.univariate) {
      res <- summary.aov(x)

      for (i in names(res)) {
        rs <- res[[i]]
        resp_name <- data.frame(
          Source = i,
          F = NA,
          df = NA,
          p.value = NA,
          stringsAsFactors = FALSE
        )
        rs <- cbind(Source = gsub(".*\\$", "", rownames(rs)),
                    rs[, c("F value", "Df", "Pr(>F)")],
                    stringsAsFactors = FALSE)
        names(rs) <- c("Source", "F", "df", "p.value")

        if (include.eta) {
          # Eta-quadrat Ueberprueft und soweit ist es korrekt
          ss <- res[[i]][, 2]
          ss1 <- ss[-length(ss)]
          sst <- ss[length(ss)]
          eta_ss <- ss1 / sst
          rs <- cbind(rs[1:3], part.eta2 =  c(eta_ss, NA), rs[4])
          resp_name <- cbind(resp_name[1:3], part.eta2 = NA, resp_name[4])
        }

        rs <- rbind(resp_name, rs)
        if (is.null(unvrt_rslt))
          unvrt_rslt <- rs
        else
          unvrt_rslt <- rbind(unvrt_rslt, rs)
      }

      if( length(rhs) == 1 ){
        unvrt_rslt <- unvrt_rslt[seq(2, nrow(unvrt_rslt), by = 3) , ]
        unvrt_rslt$Source <- gsub("Response ", "", names(res))
        names(unvrt_rslt)[1] <- "Response"
        if(include.order)
          unvrt_rslt <- unvrt_rslt[order(unvrt_rslt[[2]], decreasing = TRUE),]

        rhs <- as.character(rhs)
      } else {
        rhs <-  paste(as.character(rhs)[-1], collapse = " + ")
      }

      unvrt_rslt <- prepare_output(
        fix_data_frame_2(unvrt_rslt),
        caption = paste("Univariate F-tests by", rhs )

      )
    }


    if (include.manova) {
      muvrt_rslt <-
        broom::tidy(x, test = test) |>
        fix_format(include.rownames = FALSE, df = c("num.df", "den.df", "df"))

      if (include.eta) {
        eta <- heplots::etasq(x, test = test)
        n <- ncol(muvrt_rslt)
        muvrt_rslt <-
          cbind(muvrt_rslt[1:(n - 1)],
                part.eta2 = c( render_f(eta[, 1], 2), ""),
                muvrt_rslt[n])
      }

      muvrt_rslt <-
        prepare_output(muvrt_rslt, caption = "Multivariate Analysis of Variance", note = test)
    }



    if( include.manova & include.univariate)
      list(univariate = unvrt_rslt, manova = muvrt_rslt)
    else if( include.manova ) muvrt_rslt
    else unvrt_rslt


  }




# extract.manova <-
#   function(x,
#            test = "Wilks",
#            include.eta = FALSE,
#            ...
#      ) {
#
# caption <- "MANOVA"
# note <- ""
#     aov_result <- NULL
#     res <- summary.aov(x)
#
#     for (i in names(res)) {
#       rs <- res[[i]]
#
#       resp_name <- data.frame(
#         Source = i,
#         F = NA,
#         df = NA,
#         p.value = NA,
#         stringsAsFactors = FALSE
#       )
#       rs <- cbind(Source = gsub(".*\\$", "", rownames(rs)),
#                   rs[, c("F value", "Df", "Pr(>F)")],
#                   stringsAsFactors = FALSE)
#       names(rs) <- c("Source", "F", "df", "p.value")
#
#       if (include.eta) {
#         # Eta-quadrat Ueberprueft und soweit ist es korrekt
#         ss <- res[[i]][, 2]
#         ss1 <- ss[-length(ss)]
#         sst <- ss[length(ss)]
#         eta_ss <- ss1 / sst
#         rs <- cbind(rs[1:3], part.eta2 =  c(eta_ss, NA), rs[4])
#         resp_name <- cbind(resp_name[1:3], part.eta2 = NA, resp_name[4])
#       }
#
#       rs <- rbind(resp_name, rs)
#
#
#       if (is.null(aov_result))
#         aov_result <- rs
#       else
#         aov_result <- rbind(aov_result, rs)
#     }
#
#
#     aov_result <-
#       prepare_output(
#         fix_data_frame_2(aov_result),
#         caption = paste(caption, "Univariate F-tests"),
#         note = note,
#         rgroup = names(res),
#         n.rgroup = rep(nrow(res[[1]]), (length(res) - 1))
#       )
#
#     maov_result <- summary(x, test = test)
#     maov_result <- stp25tools::fix_to_df(maov_result$stats)
#     maov_result$Source <- gsub(".*\\$", "", maov_result$Source)
#
#     ## OK und Korrekt
#     if (include.eta) {
#       eta <- heplots::etasq(x, test = test)
#       n <- ncol(maov_result)
#       maov_result <-
#         cbind(maov_result[1:(n - 1)],
#               part.eta2 = c(eta[, 1], NA),
#               maov_result[n])
#
#     }
#
#     names(maov_result)[length(names(maov_result))] <- "p.value"
#     maov_result <-
#       prepare_output(fix_data_frame_2(maov_result),
#                      caption = paste(test, "Test"))
#
#
#
#     list(manova = aov_result, test = maov_result)
#   }



