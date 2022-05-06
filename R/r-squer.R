#' R-Quadrats
#'
#'
#' Achtung es gibt noch die Funktion caret::R2 die Probleme macht
#'
#'  Cox und Snell R2: [ 0.2 = akzeptabel, 0.4 = gut ]
#'  Nagelkerke R2: [ 0.2 = akzeptabel, 0.4 = gut, 0.5 = sehr gut]
#'  McFaddens R2: [ 0.2 = akzeptabel, 0.4 = gut ]
#' (see pscl::pR2)
#'
#' Marginal and conditional r-squared for lme objects
#'
#'
#'For mixed-effects models, R2 can be categorized into two types. Marginal R2  represents the
#'variance explained by fixed factors
#'
#'Conditional R2is interpreted as variance explained by both fixed and
#' random factors (i.e. the entire model).
#'
#'
#'
#'  \code{MuMIn::r.squaredGLMM(x, ...)}
#' Pseudo-R-squared for Generalized Mixed-Effect models
#'
#' For mixed-effects models, R² comes in two types: marginal and conditional.
#'
#' Marginal R² represents the variance explained by the fixed effects.
#'
#'
#'   Conditional R² is interpreted as a variance explained by the entire model,
#'   including both fixed and random effects.
#'
#'
#' for R2.lme an lme model (usually fit using \code{lme}
#' This method extracts the variance for fixed and random effects,
#' as well as residuals, and calls \code{rsquared.glmm}
#'
#' Marginal and conditional r-squared for merMod objects
#'
#' This method extracts the variance for fixed and random effects, residuals,
#' and the fixed effects for the null model (in the case of Poisson family),
#' and calls \code{rsquared.glmm}
#'
#' an merMod model (usually fit using lme4::lmer,
#'     lme4::glmer,lmerTest::lmer,
#'      blme::blmer, blme::bglmer, etc)
#'
#' Marginal and conditional r-squared for glmm given fixed and random variances
#'
#' This function is based on Nakagawa and Schielzeth (2013). It returns the marginal
#' and conditional r-squared, as well as the AIC for each glmm.
#' Users should call the higher-level generic "r.squared", or implement a method for the
#' corresponding class to get varF, varRand and the family from the specific object
#'
#'  return A data frame with "Class", "Family", "Marginal", "Conditional", and "AIC" columns
#'
#' @param x fit-Objekt lm glm
#' @param ... weitere Objekte nicht benutzt
#' @param varF  fot glmm Variance of fixed effects
#' @param varRand  fot glmm Variance of random effects
#' @param varResid  fot glmm Residual variance. Only necessary for "gaussian" family
#' @param family  fot glmm family of the glmm (currently works with gaussian, binomial and poisson)
#' @param link  fot glmm model link function. Working links are: gaussian: "identity" (default);
#'        binomial: "logit" (default), "probit"; poisson: "log" (default), "sqrt"
#' @param mdl.aic  fot glmm The model's AIC
#' @param mdl.class  fot glmm The name of the model's class
#' @param null.fixef  fot glmm Numeric vector containing the fixed effects of the null model.
#'        Only necessary for "poisson" family
#' @return ein data.frame-Objekt.
#'
#' @export
#' @examples
#'
#' #  fit1<-lm(chol1~chol0, hyper)
#' #  summary(fit1)$r.squared
#' #  R2(fit1)
#'
#'
R2 <- function(x, ...) {
  UseMethod("R2")

}




#' @rdname R2
#' @description APA_R2: R2-Tabelle (noch nicht fretig)
#' @export
#' @examples
#'
#' # fit1<-lm(score ~ grade + treatment, schools)
#' # fit2<-lm(score ~ grade + treatment + stdTest, schools)
#' # R2.list(fit1, fit2)
#'
R2.list <- function(..., caption, note) {
  res <- list()
  fits <- list(...)
  j <- 0
  for (i in fits) {
    j <- j + 1
    rsqr <- R2(i)
    mi <- model_info(i)
    res[[j]] <- rsqr
  }
  res
}




#' @rdname R2
#' @export
#' @examples
#'
#'  lm1 <- lm(breaks ~ wool + tension, data = warpbreaks)
#' R2(lm1)
#' broom::glance(lm1)[c( "r.squared", "adj.r.squared")]
#'
R2.lm <- function(x, ...) {
  rsq <- broom::glance(x)[1:2]
  attr(rsq, "methode") =  "r2"
  rsq
}

#' @rdname R2
#' @export
#' @description coxph:
#' 	an approximate R^2 based on Nagelkirke (Biometrika 1991)
R2.coxph <- function(x,...){
  sfit <-summary(cfit2)
  c(R2=sfit$rsq[[1]])

}


#' @rdname R2
#' @export
#' @description glm:
#'
#' McFadden:  McFadden's pseudo r-squared
#'
#'  r2ML: Cox & Snell, Maximum likelihood pseudo r-squared
#'
#'  r2CU: Nagelkerke Cragg and Uhler's pseudo r-squared
#' @examples
#'
#' lm2 <- glm(wool~ breaks + tension, warpbreaks, family= binomial())
#' R2(lm2)
#' #DescTools::PseudoR2(lm2,c("McFadden", "CoxSnell", "Nagelkerke"))
#'

R2.glm <- function(x, ...) {
  # Daniel Wollschläger Grundlagen der Datenanalyse mit R
  # glmFit<- x
  # glm0 <- update(glmFit, . ~ 1) # 0-Modell
  # LL0 <- logLik(glm0) # gesch. log-likelihood 0-Modell
  # LLf <- logLik(glmFit) # gesch. log-likelihood vollständiges Modell
  # N <- nobs(glmFit) # Anzahl der Beobachtungen
  # # R^2
  # #
  # round(c("R^2 McFadden" =as.vector(1 - (LLf / LL0)),
  #         "Cox & Snell"=  as.vector(1 - exp((2/N) * (LL0 - LLf))) ,
  #         "Nagelkerke"=  as.vector((1 - exp((2/N) * (LL0 - LLf))) / (1 - exp(LL0)^(2/N)))
  # ),4)

  rsq <- DescTools::PseudoR2(x,
                             c("McFadden", "CoxSnell", "Nagelkerke"))
  attr(rsq, "methode") =  "pseudo"
  rsq
}


#' @rdname R2
#' @export
#' @examples
#'
#'  require(MASS)
#' options(contrasts = c("contr.treatment", "contr.poly"))
#' house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
#' R2(house.plr)
R2.polr <- function(x, ...) {
  R2.glm(x,...)
}



#Type: Manova
#' @rdname R2
#' @export
R2.mlm <- function(x, ...) {
  results <-
    lapply(
      summary(x),
      FUN = function(r)
        broom::glance(r)[1:2]
    ) #"[[","r.squared")
  rsq<-broom::fix_data_frame(as.data.frame(do.call(rbind, results)))
  attr(rsq, "methode") =  "pseudo"
  rsq
}



#' @rdname R2
#' @export
R2.merMod <- function(x, ...) {

  as.data.frame(suppressWarnings(MuMIn::r.squaredGLMM(x, ...)))
#  rsq<- r.squared.merMod(x)
#  attr(rsq, "methode") =  "pseudo"
#  rsq[c("Marginal", "Conditional")]
}




#' @rdname R2
#' @export
R2.lme <- function(x, ...) {
  as.data.frame(suppressWarnings(MuMIn::r.squaredGLMM(x, ...)))
  #require(nlme)
  # rsq <- r.squared.lme(x)
  # attr(rsq, "methode") =  "pseudo"
  # rsq[c("Marginal", "Conditional")]
}

#' @noRd
# Marginal and conditional r-squared for merMod objects
r.squared.merMod <- function(mdl) {
  # Get variance of fixed effects by multiplying coefficients by design matrix
  VarF <- var(as.vector(lme4::fixef(mdl) %*% t(mdl@pp$X)))
  # Get variance of random effects by extracting variance components
  # Omit random effects at the observation level, variance is factored in later
  VarRand <- sum(sapply(lme4::VarCorr(mdl)[!sapply(unique(unlist(strsplit(
    names(lme4::ranef(mdl)), ":|/"
  ))), function(l)
    length(unique(mdl@frame[, l])) == nrow(mdl@frame))],
  function(Sigma) {
    X <- model.matrix(mdl)
    Z <- X[, rownames(Sigma)]
    sum(diag(Z %*% Sigma %*% t(Z))) / nrow(X)
  }))
  # Get the dispersion variance
  VarDisp <-
    unlist(lme4::VarCorr(mdl)[sapply(unique(unlist(strsplit(
      names(lme4::ranef(mdl)), ":|/"
    ))), function(l)
      length(unique(mdl@frame[, l])) == nrow(mdl@frame))])
  if (is.null(VarDisp))
    VarDisp = 0
  else
    VarDisp = VarDisp
  if (inherits(mdl, "lmerMod")) {
    # Get residual variance
    VarResid <- attr(lme4::VarCorr(mdl), "sc") ^ 2
    # Get ML model AIC
    # fehlerbei Modler   mdl.aic <- AIC(update(mdl, REML = F))
    #daher geändert auf
    mdl.aic <- AIC( mdl )
    # Model family for lmer is gaussian
    family <- "gaussian"
    # Model link for lmer is identity
    link <- "identity"
  }
  else if (inherits(mdl, "glmerMod")) {
    # Get the model summary
    mdl.summ <- summary(mdl)
    # Get the model's family, link and AIC
    family <- mdl.summ$family
    link <- mdl.summ$link
    mdl.aic <- AIC(mdl)
    # Pseudo-r-squared for poisson also requires the fixed effects of the null model
    if (family == "poisson") {
      # Get random effects names to generate null model
      rand.formula <- reformulate(sapply(findbars(formula(mdl)),
                                         function(x)
                                           paste0("(", deparse(x), ")")),
                                  response = ".")
      # Generate null model (intercept and random effects only, no fixed effects)
      null.mdl <- update(mdl, rand.formula)
      # Get the fixed effects of the null model
      null.fixef <- as.numeric(lme4::fixef(null.mdl))
    }
  }
  # Call the internal function to do the pseudo r-squared calculations
  rsquared.glmm(
    VarF,
    VarRand,
    VarResid,
    VarDisp,
    family = family,
    link = link,
    mdl.aic = mdl.aic,
    mdl.class = class(mdl),
    null.fixef = null.fixef
  )
}


#' @noRd
r.squared.lme <- function(mdl) {
  # Get design matrix of fixed effects from model
  Fmat <- model.matrix(eval(mdl$call$fixed)[-2], mdl$data)
  # Get variance of fixed effects by multiplying coefficients by design matrix
  VarF <- var(as.vector(nlme::fixef(mdl) %*% t(Fmat)))
  # First, extract variance-covariance matrix of random effects
  Sigma.list = nlme::VarCorr(mdl)[!grepl(" =", rownames(nlme::VarCorr(mdl))) &
                                    rownames(nlme::VarCorr(mdl)) != "Residual", colnames(nlme::VarCorr(mdl)) ==
                                    "Variance", drop = F]
  corr.list = as.numeric(nlme::VarCorr(mdl)[!grepl(" =", rownames(nlme::VarCorr(mdl))) &
                                              rownames(nlme::VarCorr(mdl)) != "Residual" &
                                              rownames(nlme::VarCorr(mdl)) != "(Intercept)", colnames(nlme::VarCorr(mdl)) ==
                                              "Corr", drop = F])
  Sigma.list2 = split(as.numeric(Sigma.list),
                      cumsum(rownames(Sigma.list) == "(Intercept)"),
                      drop = F)
  Sigma.list2 = lapply(1:length(Sigma.list2), function(i) {
    mat = matrix(
      prod(Sigma.list2[[i]]) * abs(corr.list[i]),
      ncol = length(Sigma.list2[[i]]),
      nrow = length(Sigma.list2[[i]])
    )
    diag(mat) = Sigma.list2[[i]]
    colnames(mat) = rownames(Sigma.list)[1:sum(cumsum(rownames(Sigma.list) == "(Intercept)") == 1)]
    rownames(mat) = colnames(mat)
    return(mat)
  })
  # Calculate variance of random effects
  VarRand = sum(sapply(Sigma.list2,
                       function(Sigma) {
                         Z <- Fmat[, colnames(Sigma), drop = F]
                         sum(diag(Z %*% Sigma %*% t(Z))) / nrow(Fmat)
                       }))
  # Get residual variance
  VarResid <-
    as.numeric(nlme::VarCorr(mdl)[rownames(nlme::VarCorr(mdl)) == "Residual", 1])
  # Call the internal function to do the pseudo r-squared calculations
  rsquared.glmm(
    VarF,
    VarRand,
    VarResid,
    VarDisp,
    family = "gaussian",
    link = "identity",
    mdl.aic = AIC(update(mdl, method = "ML")),
    mdl.class = class(mdl)
  )
}


#' @noRd
# Marginal and conditional r-squared for glmm given fixed and random variances
rsquared.glmm <-
  function(varF,
           varRand,
           varResid = NULL,
           varDisp = NULL,
           family,
           link,
           mdl.aic,
           mdl.class,
           null.fixef = NULL) {
    if (family == "gaussian") {
      # Only works with identity link
      if (link != "identity")
        family_link.stop(family, link)
      # Calculate marginal R-squared (fixed effects/total variance)
      Rm <- varF / (varF + varRand + varResid)
      # Calculate conditional R-squared (fixed effects+random effects/total variance)
      Rc <- (varF + varRand) / (varF + varRand + varResid)
    }
    else if (family == "binomial") {
      # Get the distribution-specific variance
      if (link == "logit")
        varDist <- (pi ^ 2) / 3
      else if (link == "probit")
        varDist <- 1
      else
        family_link.stop(family, link)
      # Calculate marginal R-squared
      Rm <- varF / (varF + varRand + varDist + varDisp)
      # Calculate conditional R-squared (fixed effects+random effects/total variance)
      Rc <- (varF + varRand) / (varF + varRand + varDist + varDisp)
    }
    else if (family == "poisson") {
      # Get the distribution-specific variance
      if (link == "log")
        varDist <- log(1 + 1 / exp(null.fixef))
      else if (link == "sqrt")
        varDist <- 0.25
      else
        family_link.stop(family, link)
      # Calculate marginal R-squared
      Rm <- varF / (varF + varRand + varDist + varDisp)
      # Calculate conditional R-squared (fixed effects+random effects/total variance)
      Rc <- (varF + varRand) / (varF + varRand + varDist + varDisp)
    }
    else
      family_link.stop(family, link)
    # Bind R^2s into a matrix and return with AIC values
    data.frame(
      Class = mdl.class,
      Family = family,
      Link = link,
      Marginal = Rm,
      Conditional = Rc,
      AIC = mdl.aic
    )
  }


#' @noRd
# stop execution if unable to calculate variance for a given family and link
family_link.stop <- function(family, link) {
  stop(paste(
    "Don't know how to calculate variance for",
    family,
    "family and",
    link,
    "link."
  ))
}



#' @rdname R2
#' @description R2: The RMSE is the square root of the variance of the residuals.
#' Compute the root mean squared error
#' (see \code{sigma})
#' @export
RMSE<- function(x, ...){
  UseMethod("RMSE")

}

#' @rdname R2
#' @export
#' @description sigma: Residual standard error  RMSE:
#'  Root Mean Square Error
#'  RMSE.lmerModLmerTest sjstats::rmse(x)
#'
RMSE.default <- function(x,...)
{



  # https://stats.stackexchange.com/questions/110999/r-confused-on-residual-terminology
  # # Mean squared error mean squared error (MSE) is the mean of the square of the residuals:
  # mse <- mean(residuals(x)^2)
  #
  # # Root mean squared error Root mean squared error (RMSE) is then the square root of MSE:
  # rmse <- sqrt(x)
  #
  # # Residual sum of squares Residual sum of squares (RSS) is the sum of the squared residuals:
  # rss <- sum(residuals(x)^2)
  #
  # # Residual standard errorResidual standard error (RSE) is the square root of (RSS / degrees of freedom):
  # sigma <- rse <- sqrt( sum(residuals(x)^2) / x$df.residual )
  #
  #



  data.frame(sigma=sigma(x),
             RMSE = sqrt(mean(x$residuals^2))

             )
}

#' @rdname R2
#' @export
RMSE.mlm <- function(x,...)
{
  broom::fix_data_frame(
    data.frame(sigma=sigma(x),
               RMSE= apply(x$residuals, 2 ,
                           FUN=function(rr) sqrt(mean(rr^2)))))
}



#' @rdname R2
#' @export
RMSE.lmerModLmerTest <- function(x, ...)
{
  data.frame(sigma = sigma(x),
             RMSE = sqrt(performance::performance_mse(x)))
}

#' @rdname R2
#' @export
RMSE.lmerMod <- function(x, ...)
{
  data.frame(sigma = sigma(x),
             RMSE = sqrt(performance::performance_mse(x)))
}

#sqrt(performance::performance_mse(fm2))
#RMSE(fm2)

# sjstats::rmse
# function (model, normalized = FALSE, verbose = TRUE)
# {
#
#     rmse_val <- sqrt(performance::performance_mse(model))
#     if (normalized) {
#       resp <- performance::.factor_to_numeric(insight::get_response(model))
#       rmse_val <- rmse_val/(max(resp, na.rm = TRUE) -
#                               min(resp,  na.rm = TRUE))
#     }
#
#
#
# }

