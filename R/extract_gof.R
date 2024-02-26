#' @rdname extract
#'
#' @param include.ftest,include.vif,include.heteroskedasticity,include.durbin,include.levene,include.bartlett
#' GOF for lm
#' @param include.loglik,include.deviance,include.minus.LL
#' GOF for glm
#' @param include.r,include.sigma,include.rmse,include.aic,include.bic,include.residual,include.normality,include.multicollin
#' GOF for lm and glm
#'
#' @export
#' @importFrom pscl pR2
#'
extract_gof <- function(x,
                            include.ftest=TRUE, include.loglik=FALSE, include.minus.LL=include.loglik,
                            include.r=FALSE, #include.pseudo=FALSE,
                            include.heteroskedasticity = TRUE,
                            include.durbin = TRUE,
                            include.levene = FALSE,
                            include.bartlett = FALSE,
                            include.vif=FALSE,
                            include.sigma=FALSE,
                            include.rmse=FALSE,
                            include.aic=TRUE, include.bic = TRUE,
                            include.residual=TRUE,
                            include.normality=FALSE,
                            include.multicollin=include.vif,

                            include.deviance=TRUE,

                            #include.obs=TRUE,
                            ...
) {
#cat("\n extract_gof \n")
  caption <- "regressions diagnostic"
  mdlnf <- model_info(x)
  type_glm  <- inherits(x, "glm")
  type_lm   <- inherits(x, "lm") & (!type_glm)
  type_lmer <- inherits(x, "lmerModLmerTest")

  xs <- if(type_lmer) summary(x) else NULL
  res <- data.frame(Test ="Obs",
                    statistic = render_f(mdlnf$N, 0),
                    stringsAsFactors=FALSE)

 # print(res)

  if (include.ftest) {
    res <-
      rbind(res,
            c(Test = "F-Statistic",
              statistic = ifelse(type_lm,
                                 APA(x,include.r = FALSE), NA)))
  }

  if (include.loglik) {
   # cat("\ninclude.loglik\n")
   # print(include.minus.LL)
    if (inherits(x, "glm")) {
      res <- rbind(res,
                   c(Test = "Likelihood Ratio Test",
                     statistic =   APA(x)))

      if (include.minus.LL) {
        minus_ll <- pscl::pR2(x)[1:3]
        minus_ll[1:2] <- minus_ll[1:2] * (-2)

        minus_ll <- render_f(minus_ll, 2)
        res <-
          rbind(res,
                c(
                  Test = "-2LL",
                  statistic =  paste0("LL=", minus_ll[1],
                                      ", LL-Null=", minus_ll[2],
                                      ", G2=", minus_ll[3])
                ))

      #  print(res)

      }
    } else{
      res <- rbind(res,
                   c(Test = "Likelihood Ratio Test",
                     statistic =   NA))

      if (include.minus.LL) {
        res <-
          rbind(res,
                c(Test = "-2LL", statistic = NA))
      }
    }
  }

  if (include.deviance) {
    res <- rbind(res,
                 c(Test = "Deviance Residuals",
                   statistic =   render_f(deviance(x,REML=FALSE), 1)))

  }

  if (include.r) {
    # res <-rbind(res,
    #             c(Test = "R-Squared",
    #               statistic = ifelse( type_lm,
    #                                   rndr_r2(R2(x)),
    #                                   ifelse( type_glm | type_lmer,
    #                                           rndr_r2pseudo(R2(x)),
    #                                           NA))))


    r_sqrt <- R2(x)
    if( type_lm ) names(r_sqrt) <- c("R2", "adj. R2")
    res <- rbind(res,
                 cbind(
                   Test = names(r_sqrt),
                   statistic = as.character(render_f(r_sqrt, digits = 2))
                 ))

  }

  if (include.heteroskedasticity) {
    res <-
      rbind(res,
            c(Test = "Heteroskedasticity (Breusch-Pagan)",
              statistic = ifelse( type_lm |type_glm, APA(lmtest::bptest(x)), NA )
            ))
  }

  if (include.durbin) {
    res <-
      rbind(res,
            c(Test = "Autocorrelation (Durbin-Watson)",
              statistic = ifelse( type_lm |type_glm, APA(lmtest::dwtest(x)), NA )
            ))
  }

  if (include.levene) {
    levi <- car::leveneTest(x$model[, 1], x$model[, 2])
    levi <-  rndr_F(levi[1, 2],  levi[1, 1],  levi[2, 1],  levi[1, 3])

    res <- rbind(res,
                 c(Test = "Homogeneity of Variances (Levene's)",
                   statistic = levi))
  }

  if (include.bartlett) {
    res <-
      rbind(res,
            c(Test = "Homogeneity of Variances (Bartlett)",
              statistic =  APA(stats::bartlett.test(x$model[,1], x$model[,2]))))

  }

  if (include.normality) {
    res <-
      rbind(res,
            c(Test = "Shapiro-Wilk normality test",
              statistic =  test_normality(x)))
  }

  if (include.multicollin) {
    res <-
      rbind(res,
            c(Test = "Autocorrelation (VIF)",
              statistic =  test_multicollin(x)))
  }

  if (include.aic) {
    res <-
      rbind(res,
            c(Test = "AIC",
              statistic = render_f(AIC(x),1)  ))

  }

  if (include.bic) {
    res <-
      rbind(res,
            c(Test = "BIC",
              statistic = render_f(BIC(x),1)  ))

  }

  if (include.rmse) {
    res <-
      rbind(res,
            c(Test =  "RMSE",
              statistic = render_f(RMSE(x)[1,2],2)
            ))
  }

  if (include.sigma) {
    res <-
      rbind(res,
            c(Test = "Sigma",
              statistic = ifelse( type_lm |type_glm, render_f(RMSE(x)[1,1],2),
                                  ifelse( type_lmer,  render_f(xs$sigma, 2)))

            ))
  }

  if (include.residual) {
    res <-
      rbind(res,
            c(Test = "Var: Residual",
              statistic = ifelse( type_lm |type_glm, render_f(RMSE(x)[1,1]^2,2),
                                  ifelse( type_lmer,  render_f(xs$sigma^2, 2)))

            ))
  }

#                                  Test                                statistic
# 2                         F-Statistic                   F(6, 276)=2.14, p=.049
# 3                  Deviance Residuals                                     31.3
# 4
# 5  Heteroskedasticity (Breusch-Pagan)                       BP(6)=4.63, p=.592
# 6     Autocorrelation (Durbin-Watson)                          DW=2.28, p=.984
# 7         Shapiro-Wilk normality test                           W=0.96, p<.001
# 8                                 AIC                                    195.9
# 9                                 BIC                                    225.0
# 10                      Var: Residual                                     0.11
# 1                                 Obs                                      283

 # print(res )
  rbind(res[-1,], res[1,])

}

#' @noRd
test_normality <- function(x) {
  # bei  sjstats ist im orginal stats::rstandard ich verwende aber resid
  APA( stats::shapiro.test(stats::resid(x)) )
}

#' @noRd
test_multicollin <- function(x) {
  x <- performance::check_collinearity(x)
  vifs <- x$VIF
  ifelse(
    any(vifs >= 10) ,
    "VIF>10 high correlation",
    ifelse(
      any(vifs >= 5 &
            vifs < 10),
      "VIF>5 moderate correlation",
      "VIF<5 low correlation"
    )
  )
}
