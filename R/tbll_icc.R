#' Intraclass Correlation Coefficient (ICC)
#'
#' stolen from performance::icc
#'
#' @export
#'
#' @examples
#'
#'  fm1 <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'  Tbll_icc(fm1)
#'
Tbll_icc <- function(x, ...) {
  UseMethod("Tbll_icc")
}


#' @rdname Tbll_icc
#'
#' @param ...  lmer - fits
#' @param names,digits an prepare data
#' @param by_group,tolerance,ci,iterations geht an icc {performance}
#' @return data.frame
#' @export
#'
Tbll_icc.lmerMod  <-
  function(...,
           names = NULL,
           digits = 2,
           by_group = FALSE,
           tolerance = 1e-05,
           ci = NULL,
           iterations = 100) {
    if (is.null(names))
      names <-  paste(as.list(sys.call())[-1])
    dots <- list(...)
    rslt <- NULL

    for (i in seq_along(dots)) {
      rst <- performance::icc(dots[[i]])
      class(rst) <- "data.frame"
      rst <- cbind(Source = names[i], render_f(rst, digits = digits))
      rslt <- rbind(rslt, rst)
    }
    prepare_output(rslt, caption =  "Intraclass Correlation Coefficient")
  }



#' @rdname Tbll_icc
#' @export
Tbll_icc.default <- function(...) {
  stp25metcomp::Tbll_icc(...)
}




# performance::icc <-
# function (model, by_group = FALSE, tolerance = 1e-05, ci = NULL,
#           iterations = 100, ...)
# {
#   if (inherits(model, "sem") && inherits(model, "lme")) {
#     return(model$icc)
#   }
#   if (insight::is_multivariate(model)) {
#     if (inherits(model, "brmsfit")) {
#       return(variance_decomposition(model))
#     }
#     else {
#       insight::print_color("Multiple response models not yet supported. You may use `performance::variance_decomposition()`.\n",
#                            "red")
#       return(NULL)
#     }
#   }
#   if (!insight::is_mixed_model(model)) {
#     warning("`model` has no random effects.", call. = FALSE)
#     return(NULL)
#   }
#   vars <- .compute_random_vars(model, tolerance)
#   if (is.null(vars) || all(is.na(vars))) {
#     return(vars)
#   }
#   if (isTRUE(by_group)) {
#     if (!is.null(insight::find_random_slopes(model))) {
#       insight::format_warning("Model contains random slopes. Cannot compute accurate ICCs by group factors.")
#     }
#     if (!is.null(ci) && !is.na(ci)) {
#       insight::format_warning("Confidence intervals are not yet supported for `by_group = TRUE`.")
#     }
#     icc_overall <- vars$var.intercept/(vars$var.random +
#                                          vars$var.residual)
#     out <- data.frame(Group = names(icc_overall), ICC = unname(icc_overall),
#                       stringsAsFactors = FALSE)
#     class(out) <- c("icc_by_group", class(out))
#   }
#   else {
#     icc_adjusted <- vars$var.random/(vars$var.random + vars$var.residual)
#     icc_unadjusted <- vars$var.random/(vars$var.fixed + vars$var.random +
#                                          vars$var.residual)
#     out <- data.frame(ICC_adjusted = icc_adjusted, ICC_conditional = icc_unadjusted,
#                       ICC_unadjusted = icc_unadjusted)
#     if (!is.null(ci) && !is.na(ci)) {
#       result <- .bootstrap_icc(model, iterations, tolerance,
#                                ...)
#       icc_ci_adjusted <- as.vector(result$t[, 1])
#       icc_ci_adjusted <- icc_ci_adjusted[!is.na(icc_ci_adjusted)]
#       icc_ci_adjusted <- bayestestR::eti(icc_ci_adjusted,
#                                          ci = ci)
#       icc_ci_unadjusted <- as.vector(result$t[, 2])
#       icc_ci_unadjusted <- icc_ci_unadjusted[!is.na(icc_ci_unadjusted)]
#       icc_ci_unadjusted <- bayestestR::eti(icc_ci_unadjusted,
#                                            ci = ci)
#       out_ci <- data.frame(ICC_adjusted = c(CI_low = icc_ci_adjusted$CI_low,
#                                             CI_high = icc_ci_adjusted$CI_high), ICC_conditional = c(CI_low = icc_ci_unadjusted$CI_low,
#                                                                                                     CI_high = icc_ci_unadjusted$CI_high), ICC_unadjusted = c(CI_low = icc_ci_unadjusted$CI_low,
#                                                                                                                                                              CI_high = icc_ci_unadjusted$CI_high))
#       out <- rbind(out, out_ci)
#       attr(out, "ci") <- ci
#     }
#     class(out) <- c("icc", "data.frame")
#   }
#   out
# }
