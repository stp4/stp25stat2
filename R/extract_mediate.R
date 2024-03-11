#' Tbll_mediate
#'
#' Causal Mediation Analysis
#'
#' Eine Mediation beschreibt die Wirkung einer Einfluss Variablen (X) auf eine
#' Zielvariable Variable (Y) über die Zwischenstufe einer Mediatorvariablen (M).
#' oder auch unabhängige Variable > Mediator > abhängige Variable.
#'
#'
#' Einfache Mediation und Moderationsanalyse mit Sobel-Test. Mediation ist gegeben wenn alle Modelle signifikant (1) und (4)
#'
#' Y ~ X
#' Y ~ X + M
#' (wenn hier M signifikant ist => partielle Mediation) (4) M ~ X
#'
#' Moderation ist gegeben wenn die Interaktion (X:M) signifikant ist
#'
#' Y ~ X + M + X:M
#'
#'
#' Quelle: https://bjoernwalther.com/mediation-in-r/
#'
#'  Sobel-Test:
#' Ein Beispiel mit Laavan findet sich unter
#' https://paolotoffanin.wordpress.com/2017/05/06/multiple-mediator-analysis-with-lavaan/
#'
#'
#' @name Tbll_mediate
#'
#' @param ... weitere Einstellungen
#'
#' @return data.frame
#'
#' @examples
#'
#'
#'
#' # install.packages("mediation")
#' #library(mediation)
#' #require(stp25stat2)
#' #require(stp25output2)
#'
#'
#' # Data --------------------------------------------------------------------
#'
#'
#' set.seed(1234)
#' n <- 2 * 15
#' #
#' # Note ~ Motivation
#' # Note ~ Motivation + Lerndauer
#' # Lerndauer ~ Motivation
#' scale2 <- function(x) {
#'   x <- scale(x)
#'   x <- x  - min(x)
#'   round(x / max(x) * 4 + 1 , 1)
#' }
#'
#' Motivation <-  scale2(rnorm(n))
#' Lerndauer <-  scale2(1+Motivation + rnorm(n,0,1))
#' Note <- scale2(1 +   2.9 * Lerndauer + 3.3 * Motivation + rnorm(n, 0, 1) )
#'
#' dat <- data.frame(
#'   id = 1:n,
#'   sex = gl(2, n / 2, labels = c("m", "f")),
#'   Note = Note,
#'   Lerndauer = Lerndauer,
#'   Motivation = Motivation
#' )
#'
#' dat |> Tbll_desc_item(Note, Motivation , Lerndauer)
#'
#'
#' # library mediation -------------------------------------------------------
#' pfad_x <- lm(Note ~ Motivation , dat)
#' # Direkter Effekt von X auf Y und Effekt von M auf Y
#' pfad_xm_y <- lm(Note ~ Motivation + Lerndauer, dat)
#' pfad_x_i_m_y <- lm(Note ~ Motivation * Lerndauer, dat)
#' # Direkter Effekt von X auf M
#' pfad_x_m <- lm(Lerndauer ~ Motivation , dat)
#'
#' Tbll_reg(pfad_x, pfad_x_m, pfad_xm_y, pfad_x_i_m_y , include.ci = FALSE)
#'

#'
#' pfad_x_y <- lm(Note ~ Motivation , dat)
#' pfad_x_m_y <- lm(Note ~ Motivation + Lerndauer, dat)
#' # Direkter Effekt von X auf M
#' pfad_x_m <- lm(Lerndauer ~ Motivation , dat)
#'
#' # pfad_x_y <- lmerTest::lmer(Note ~ Motivation +time+ (1 | id), dat)
#' # pfad_x_m_y <- lmerTest::lmer(Note ~ Motivation + Lerndauer +time+ (1 | id), dat)
#' # pfad_x_m <- lmerTest::lmer(Lerndauer ~ Motivation +time+ (1 | id), dat)
#'
#' Sobel_Test(pfad_x_y, pfad_x_m_y, pfad_x_m,
#'            treat="Motivation",
#'            mediator="Lerndauer" )
#'
#'
#'
#' # library(mediation)
#' # rslt <- mediation::mediate(pfad_x_m, pfad_xm_y,
#' #                            treat = "Motivation", mediator = "Lerndauer",
#' #                            boot = FALSE,
#' #                            sims = 100)
#' # summary(rslt)
#' # Tbll(rslt)
#' # #
#' # #  indirekten Effekt
#' # #ACME ist der average causal mediation effect. Das ist der indirekte Effekt (Pfad X und M)
#' # pfad_x_m$coefficients["Motivation"] * pfad_xm_y$coefficients["Lerndauer"]
#'
#'
#'
#' # library(psych)
#' #
#' # mod4 <-
#' #   psych::mediate(
#' #     Note ~ Motivation + (Lerndauer),
#' #     data = dat,
#' #     n.iter = 50,
#' #     std = FALSE,
#' #     plot = FALSE
#' #   )
#' # # mod4 <- psych::mediate(Note ~ Motivation + Motivation:Lerndauer + (Lerndauer), data =dat, n.iter=50)
#' # class(mod4)
#' #
#' # print(mod4, short = TRUE )
#' # # summary(mod4)
#' # #
#' # #  mediate.diagram(mod4)
#' # #  moderate.diagram(mod4)
#' # #
#' # # summary(mod4,plot=FALSE)
#' #
#' # Tbll(mod4) # |> Output()
#' # Tbll(rslt)
NULL






#' @rdname extract
#' @description
#' Causal Mediation Analysis:
#'  stolen from mediate {mediation} and psych {mediate}
#'
tbll_extract.mediate  <- function(x, ...) {
  if (any(class(x) == "psych"))
    summary_psych_mediate(x, ...)
  else
    tbll_extract.summary.mediate(summary(x, ...))
}


#' @rdname extract
#' @description tbll_extract.summary.mediate: extract Output from mediate:::summary.mediate
tbll_extract.summary.mediate <-
  function (x,
            digits = 2,
            ...)  {
caption <- ""
note <-  ""
    clp <- 100 * x$conf.level
    caption <-
     sprintf(
        "Causal Mediation Analysis %s",
        ifelse(
          inherits(x,"mediate.tsls"),
          "using Two-Stage Least Squares", "")
      )


    if (x$boot) {
      caption <-
          sprintf(
            "Nonparametric Bootstrap Confidence Intervals with the %s Method",
            ifelse(x$boot.ci.type == "perc", "Percentile", "BCa")
          )
    }
    else {
      caption <- (sprintf(
        "%s Confidence Intervals",
        ifelse(
          inherits(x,
                   "mediate.tsls"),
          "Two-Stage Least Squares",
          "Quasi-Bayesian"
        )
      ))
    }
    if (!is.null(x$covariates)) {
      caption <-
        ("(Inference Conditional on the Covariate Values Specified in `covariates')")
    }
    isLinear.y <- ((class(x$model.y)[1] %in% c("lm", "rq")) ||
                     (
                       inherits(x$model.y, "glm") && x$model.y$family$family ==
                         "gaussian" &&
                         x$model.y$family$link == "identity"
                     ) ||
                     (
                       inherits(x$model.y, "survreg") && x$model.y$dist ==
                         "gaussian"
                     )
    )
    printone <- !x$INT && isLinear.y
    if (printone) {
      smat <- c(x$d1, x$d1.ci, x$d1.p)
      smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
      smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
      smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
      rownames(smat) <-
        c("ACME", "ADE", "Total Effect", "Prop. Mediated")
    }
    else {
      smat <- c(x$d0, x$d0.ci, x$d0.p)
      smat <- rbind(smat, c(x$d1, x$d1.ci, x$d1.p))
      smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
      smat <- rbind(smat, c(x$z1, x$z1.ci, x$z1.p))
      smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
      smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
      smat <- rbind(smat, c(x$n1, x$n1.ci, x$n1.p))
      smat <- rbind(smat, c(x$d.avg, x$d.avg.ci, x$d.avg.p))
      smat <- rbind(smat, c(x$z.avg, x$z.avg.ci, x$z.avg.p))
      smat <- rbind(smat, c(x$n.avg, x$n.avg.ci, x$n.avg.p))
      rownames(smat) <- c(
        "ACME (control)",
        "ACME (treated)",
        "ADE (control)",
        "ADE (treated)",
        "Total Effect",
        "Prop. Mediated (control)",
        "Prop. Mediated (treated)",
        "ACME (average)",
        "ADE (average)",
        "Prop. Mediated (average)"
      )
    }
    colnames(smat) <- c(
      "Estimate",
      paste(clp, "% CI Lower", sep = ""),
      paste(clp, "% CI Upper", sep = ""),
      "p-value"
    )

    note <-
      paste("Sample Size Used:", x$nobs, "Simulations:", x$sims)

    prepare_output(
      cbind(
        Effect = row.names(smat),
        render_f(smat[, 1:3], digits),
        'p Value ' = rndr_P(smat[, 4], FALSE)
      ),
      caption = caption,
      note = note
    )
  }



#finally got the print to work on multiple dvs    11/24/19

summary_psych_mediate <- function(x, digits = 2, short = FALSE) {
  rslt <- list()
#  cat("Call: ")
 # print(x$Call)
  dv <- x$var.names[["DV"]]
  # iv <- x$var.names[["IV"]]
  mv <- x$var.names[["med"]]
  mod <- x$var.names[["mod"]]
  # dv <- x$names[1]
  iv <- rownames(x$direct)[-1]
  niv <- length(iv)
  nmed <- length(mv)
  ndv <- length(dv)
  nz <- length(x$var.names[["z"]])

  if (nmed < 1) {
    caption<- "No mediator specified leads to traditional regression "
  } else {
    caption<- "Direct effect estimates (traditional regression) (c') X + M on Y"
  }

  #print the traditional regression values
  for (j in 1:ndv) {
    if (niv == 1) {
      dfd <-

          data.frame(
            direct = x$cprime.reg$beta[, j],
            se = x$cprime.reg$se[, j],
            t = x$cprime.reg$t[, j],
            df = x$cprime.reg$df
          )
      dfdp <-
        cbind(dfd, p = x$cprime.reg$prob[, j])
    } else {
      dfd <-
          data.frame(
            direct = x$cprime.reg$beta[1:(niv + 1 + nmed), j],
            se = x$cprime.reg$se[1:(niv + 1 + nmed), j],
            t = x$cprime.reg$t[1:(niv + 1 + nmed), j],
            df = x$cprime.reg$df
        )

      dfdp <-
        cbind(dfd, p = x$cprime.reg$prob[1:(niv + 1 + nmed), j])
    }
    colnames(dfdp) <- c(dv[j], "se", "t", "df", "Prob")


    Fx <-
      x$cprime.reg$df * x$cprime.reg$R2[j] / (((nrow(x$cprime.reg$beta) - 1) * (1 - x$cprime.reg$R2[j])))
    pF <- -expm1(pf(Fx, nrow(x$cprime.reg$beta) - 1, x$cprime.reg$df, log.p = TRUE))

      note<-
        paste("R =",  round(sqrt(x$cprime.reg$R2[j]), digits),
              "R2 =", round(x$cprime.reg$R2[j], digits),
              "F =",  round(Fx, digits),
              "on",   nrow(x$cprime.reg$beta) - 1,
              "and",  x$cprime.reg$df,
              "DF p-value: ", signif(pF, 3))

  rslt$dp_1111 = prepare_output(
    cbind(
      Parameter = row.names(dfdp),
      render_f(dfdp[, 1:3], digits),
      df =  render_f(dfdp[, 4], 0),
      'p' = rndr_P(dfdp[, 5], FALSE)
    ),
    caption=caption,
    note = note
  )


  }




  if (nmed > 0) {
    caption <- "Total effect estimates (c) (X on Y)"

    for (j in 1:ndv) {
      dft <-

          data.frame(
            direct = x$total.reg$beta[, j],
            se = x$total.reg$se[, j],
            t = x$total.reg$t[, j],
            df = x$total.reg$df
        )
      dftp <-
        cbind(dft, p = x$total.reg$prob[, j])
      colnames(dftp) <- c(dv[j], "se", "t", "df", "Prob")
      rownames(dftp) <- rownames(x$total.reg$beta)
    #  print(dftp)

      rslt$tp_1111 <-
      prepare_output(
        cbind(
          Parameter = row.names(dftp),
          render_f(dftp[, 1:3], digits),
          df =  render_f(dftp[, 4], 0),
          'p' = rndr_P(dftp[, 5], FALSE)
        ),
        caption=caption,
        note = ""
      )
    }



    caption <- "'a' effect estimates (X on M)"
    for (j in 1:ndv) {
      if (niv == 1) {
        for (i in 1:nmed) {
          dfa <-
              data.frame(
                a = x$a.reg$beta[, i],
                se = x$a.reg$se[, i],
                t = x$a.reg$t[, i],
                df = x$a.reg$df
            )
          dfa <-
            cbind(dfa, p = x$a.reg$prob[, i])
          if (NROW(dfa) == 1) {
            rownames(dfa) <- rownames(x$a.reg$beta)
            colnames(dfa) <-
              c(colnames(x$a.reg$beta), "se", "t", "df", "Prob")
          } else {
            rownames(dfa) <- rownames(x$a.reg$beta)
            colnames(dfa) <-
              c(colnames(x$a.reg$beta)[i], "se", "t", "df", "Prob")
          }

          rslt$a <-
          prepare_output(
            cbind(
              Parameter = row.names(dfa),
              render_f(dfa[, 1:3], digits),
              df =  render_f(dfa[, 4], 0),
              'p' = rndr_P(dfa[, 5], FALSE)
            ),
            caption=caption,
            note = ""
          )
        }
      }
      else {
        for (i in 1:nmed) {
          dfa <-
              data.frame(
                a = x$a.reg$beta[, i],
                se = x$a.reg$se[, i],
                t = x$a.reg$t[, i],
                df = x$a.reg$df
            )
          dfa <-
            cbind(dfa, p = x$a.reg$prob[, i])
          rownames(dfa) <- rownames(x$a.reg$beta)
          colnames(dfa) <-
            c(colnames(x$a.reg$beta)[i], "se", "t", "df", "Prob")
          #print(dfa)

          rslt$a <-
            prepare_output(
              cbind(
                Parameter = row.names(dfa),
                render_f(dfa[, 1:3], digits),
                df =  render_f(dfa[, 4], 0),
                'p' = rndr_P(dfa[, 5], FALSE)
              ),
              caption=caption,
              note = ""
            )



        }

      }
    }


    caption <- "b'  effect estimates (M on Y controlling for X)"
    for (j in 1:ndv) {
      if (niv == 1) {
        dfb <-
            data.frame(
              direct = x$b.reg$beta[-(1:niv), j],
              se = x$b.reg$se[-(1:niv), j],
              t = x$b.reg$t[-(1:niv), j],
              df = x$b.reg$df
          )
        dfb <-
          cbind(dfb,
                p = x$b.reg$prob[-(1:niv), j])
      }
      else {
        dfb <-
            data.frame(
              direct = x$b.reg$beta[-(1:niv), j],
              se = x$b.reg$se[-(1:niv), j],
              t = x$b.reg$t[-(1:niv), j],
              df = x$b.reg$df
          )
        dfb <-
          cbind(dfb,
                p = x$b.reg$prob[-(1:niv), j])
      }


      rownames(dfb) <- rownames(x$b.reg$beta)[-(1:niv)]
      colnames(dfb) <-  c(dv[j], "se", "t", "df", "Prob")
     # print(dfb)

      rslt$b <-
        prepare_output(
          cbind(
            Parameter = row.names(dfb),
            render_f(dfb[, 1:3], digits),
            df =  render_f(dfb[, 4], 0),
            'p' = rndr_P(dfb[, 5], FALSE)
          ),
          caption=caption,
          note = ""
        )

    }




    #not clear how this is different from next section
    #the indirect is correct, but the mediators (boot) need to be summed
    caption <- "'ab' effect estimates (through all  mediators)"


    #need to think about the number of mediators as well as ndv and niv
    for (j in 1:ndv) {
      #currently only works for ndv =1
      if (niv > 1) {
        dfab  <-
            data.frame(
              indirect = x$ab[, j],
              boot = x$boot$mean[, j],
              sd = x$boot$sd[, j],
              #    lower=x$boot$ci[,((j-1)*(niv+ndv+nmed)+niv+1):((j)*(niv+ndv+nmed))],
              #  upper=x$boot$ci[,((j-1)*(niv+ndv+nmed)+niv+1):((j)*(niv+ndv+nmed))]),digits)}  else {
              lower = x$boot$ci[1, j],
              upper = x$boot$ci[2, j]

          )
      }  else {
        dfab  <-
          data.frame(
            indirect = x$ab[, j],
            boot = x$boot$mean[, j]
            ,
            sd = x$boot$sd[, j],
            #                         #  lower=x$boot$ci[1,1:niv],   #was niv perhaps should be ndv?
            #                          #  upper=x$boot$ci[2,1:niv]),digits)
            lower = x$boot$ci[1, (1:niv + niv *
                                    (j - 1))],
            upper = x$boot$ci[2, (1:niv + niv *
                                    (j - 1))]
          )
        #                          #  lower=x$boot$ci[1,(j*niv )],
        #                           # upper=x$boot$ci[2,(j*niv )]),digits)
        # dfab <- round(data.frame(indirect = x$ab[,j],boot = sum(x$boot$mean[,1:(j*niv+1)])),digits)}
      }
      rownames(dfab) <- rownames(x$ab)
      colnames(dfab)[1] <- dv[j]
    #  print("round(dfab, digits)")
    #  rslt$dfab <- dfab

      rslt$ab <-
        prepare_output(
          cbind(
            Parameter = row.names(dfab),
            render_f(dfab[, 1:3], digits),
            df =  render_f(dfab[, 4], 0),
            'p' = rndr_P(dfab[, 5], FALSE)
          ),
          caption=caption,
          note = ""
        )
    }
    #      }

    #now show the individual ab effects (just works for 1 dv)


    #rownames(x$boot$ci) <- rownames(x$boot$mean) <- rownames(x$boot$sd ) <- NULL

    #problem when ndv > 1



    for (k in 1:ndv) {
      if (nmed > 1) {
        caption <-
          paste("'ab' effects estimates for each mediator for",
            colnames(x$ab)[k])
        #rows of x$boot$mean are IVs
        #columns of x$boot$mean are g for each DV
        dfab <-
            data.frame(
              boot = as.vector(x$boot$mean),
              sd = as.vector(x$boot$sd),
              lower = x$boot$ci[1, ],
              upper = x$boot$ci[2, ]
            )



        rslt$dfab <-
          prepare_output(
            cbind(
              Parameter = row.names(dfab),
              render_f(dfab[, 1:5], digits)#,
            #  df =  render_f(dfab[, 4], 0),
           #   'p' = rndr_P(dfab[, 5], FALSE)
            ),
            caption=caption,
            note = ""
          )
        #now, if number of mediators >1, compare them
      }


    }

  }
#cat("\n\n-----------------------\n")
  rslt$summary <-   print_psych_mediate(x)
  rslt


}



print_psych_mediate <-  function(x, digits=2 ) {

  rslt <- data.frame(
    Effect = rep(NA, 4),
    Estimate = rep(NA, 4),
    SE = rep(NA, 4),
    T = rep(NA, 4),
    df = rep(NA, 4),
    p = rep(NA, 4)
  )
  # cat("\nMediation/Moderation Analysis \nCall: ")
  # print(x$Call)
  dv <- x$var.names[["DV"]]
  # iv <- x$var.names[["IV"]]
  mv <- x$var.names[["med"]]
  mod <- x$var.names[["mod"]]
  # dv <- x$names[1]
  iv <- rownames(x$direct)
  if (iv[1] == "Intercept")  iv <- iv[-1]
  niv <- length(iv)
  nmed <- length(mv)
  ndv <- length(dv)
  nz <- length(x$var.names[["z"]])

  # if(dim(x$a)) {mv <- names(x$a)} else {mv <- colnames(x$a)
  caption <- paste0("The DV (Y) was ", dv,". The IV (X) was ", iv,". The mediating variable(s) = ", mv,".")


  if(!is.null(x$mod))  caption <- paste0( caption,  "  The moderating variable(s) = ",mod)
  if(!is.null(x$var.names$z))  aption <- paste0( caption, " Variable(s)  partialled out were", x$var.names[["z"]])


  #print(caption)
  # cat("\n------------\n")

  if (!is.null(mv)) {
    for (j in 1:ndv) {
      for (i in 1:niv) {
        rslt<-
          data.frame(
            Effect =  "Total effect" ,#(c) of ",iv[i]  ), #" on ",dv[j]),
            Estimate = render_f(x$direct[i + 1, j], digits),
            SE = render_f(x$total.reg$se[i + 1, j], digits),
            T =  render_f(x$total.reg$t[i + 1, j], digits),
            df =  render_f(x$total.reg$df, 0),
            p = rndr_P(x$total.reg$prob[i + 1, j], FALSE))




        rslt <-
          rbind(rslt,
                data.frame(
                  Effect = "Direct effect (ADE)" ,  # (c') of ", iv[i], " on ", dv[j], " removing ", mv),
                  Estimate = render_f(x$cprime.reg$beta[i + 1, j], digits),
                  SE = render_f(x$cprime.reg$se[i + 1, j] , digits),
                  T =  render_f(x$cprime.reg$t[i + 1, j], digits),
                  df =  render_f(x$cprime.reg$df, 0),
                  p = rndr_P(x$cprime.reg$prob[i + 1, j] , FALSE)

                ))


        if (is.null(x$mod)) {

          rslt <- rbind(rslt,
                        data.frame(
                          Effect = "Indirect effect (ab)",
                          Estimate = render_f(x$ab[i, j], digits),
                          SE =  NA,
                          T =  NA,
                          df = NA,
                          p = NA
                        ))


          rslt <- rbind(rslt,
                        data.frame(
                          Effect = "Mean bootstrapped indirect effect",
                          Estimate = render_f(x$boot$mean[i], digits),
                          SE =  render_f(x$boot$sd[i] , digits),
                          T =  NA,
                          df = NA,
                          p = NA))


          Fx <-
            x$cprime.reg$df * x$cprime.reg$R2[j] / (((nrow(
              x$cprime.reg$beta
            ) - 1) * (1 - x$cprime.reg$R2[j])))
          pF <-
            -expm1(pf(Fx, nrow(x$cprime.reg$beta), x$cprime.reg$df, log.p = TRUE))
          note <- paste0(
            "R = ",round(sqrt(x$cprime.reg$R2[j]), 2),
            ", R2 = ",round(x$cprime.reg$R2[j], 2),

            ", F(",nrow(x$cprime.reg$beta) - 1, ", ",x$cprime.reg$df, ") = ",round(Fx, 2),
            ", ",  rndr_P(pF, digits + 1)  )
        }
      }
    }
  }
  prepare_output(rslt, caption=caption, note=note)


}

#
# #install.packages("mediation")
# library(mediation)
# require(stp25stat2)
# require(stp25output2)
#
#
# set.seed(1234)
# n <- 2 * 15
# #
# # Note ~ Motivation
# # Note ~ Motivation + Lerndauer
# # Lerndauer ~ Motivation
# scale2 <- function(x) {
#   x <- scale(x)
#   x <- x  - min(x)
#   round(x / max(x) * 4 + 1 , 1)
# }
#
# Motivation <-  scale2(rnorm(n))
# Lerndauer <-  scale2(1+Motivation + rnorm(n,0,1))
# Note <- scale2(1 +   2.9 * Lerndauer + 3.3 * Motivation + rnorm(n, 0, 1) )
#
# dat <- data.frame(
#   id = 1:n,
#   sex = gl(2, n / 2, labels = c("m", "f")),
#   Note = Note,
#   Lerndauer = Lerndauer,
#   Motivation = Motivation
# )
#
# dat |> Tbll_desc_item(Note, Motivation , Lerndauer)
#
#
#
#
#
# pfad_x <- lm(Note ~ Motivation , dat)
#
# # Direkter Effekt von X auf Y und Effekt von M auf Y
# pfad_xm_y <- lm(Note ~ Motivation + Lerndauer, dat)
# pfad_x_i_m_y <- lm(Note ~ Motivation * Lerndauer, dat)
# # Direkter Effekt von X auf M
# pfad_x_m <- lm(Lerndauer ~ Motivation , dat)
#
# Tbll_reg(pfad_x, pfad_x_m, pfad_xm_y, pfad_x_i_m_y , include.ci = FALSE)
#
# library(mediation)
# rslt <- mediation::mediate(pfad_x_m, pfad_xm_y,
#                            treat = "Motivation", mediator = "Lerndauer",
#                            boot = FALSE,
#                            sims = 100)
# summary(rslt)
# Tbll(rslt)
# #
# #  indirekten Effekt
# #ACME ist der average causal mediation effect. Das ist der indirekte Effekt (Pfad X und M)
# pfad_x_m$coefficients["Motivation"] * pfad_xm_y$coefficients["Lerndauer"]
#
# library(psych)
#
# mod4 <-
#   psych::mediate(
#     Note ~ Motivation + (Lerndauer),
#     data = dat,
#     n.iter = 50,
#     std = FALSE,
#     plot = FALSE
#   )
# # mod4 <- psych::mediate(Note ~ Motivation + Motivation:Lerndauer + (Lerndauer), data =dat, n.iter=50)
# class(mod4)
#
# print(mod4, short = TRUE )
# # summary(mod4)
# #
# #  mediate.diagram(mod4)
# #  moderate.diagram(mod4)
# #
# # summary(mod4,plot=FALSE)
#
# Tbll(mod4) # |> Output()
# Tbll(rslt)


#' @rdname Tbll_mediate
#'
#' @param y_x,y_xm,m_x lm -Objekte
#' @param treat Treatment
#' @param mediator  Mediator
#' @param digits Nachkomastellen
#'
#' @return data.frame
#' @export
Sobel_Test <- function(y_x, y_xm, m_x,
                       treat, mediator,
                       digits = 2,
                       ...) {

  if( (length(mediator)>1) | (length(treat)>1) ){
    stop()
  }

  mod1.out <- summary(y_x)$coef
  mod2.out <- summary(y_xm)$coef
  mod3.out <- summary(m_x)$coef

  pred <- which(rownames(mod3.out) == treat)
  med <- which(rownames(mod2.out) == mediator)



  indir <- mod3.out[pred, 1] * mod2.out[med, 1]

  effvar <- (mod3.out[pred, 1]) ^ 2 * (mod2.out[med, 2]) ^ 2 +
    (mod2.out[med, 1]) ^ 2 * (mod3.out[pred, 2]) ^ 2
  serr <- sqrt(effvar)
  zvalue = indir / serr

  data.frame(
    Source = "indir",
    B = render_f(indir, digits = digits),
    SE = render_f(serr, digits = digits),
    Z = render_f(zvalue, 1),
    p.value = rndr_P(2 * pnorm(-abs(zvalue)), FALSE),
    row.names = "Sobel",
    stringsAsFactors = FALSE
  )

}
