#' Extract partial effects
#'
#' effects:   Extract partial effects obtained with allEffects()
#'
#'
#' Irgendas ist hier faul!!!
#'
#' @rdname extract
#' @param term  effects: x-var
#' @param response effects: y-var
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#'  require(effects)
#'
#' # data(Prestige, package = "carData")
#' # mod <- lm(log(prestige) ~ income:type + education, data=Prestige)
#' # eff = effect("education", mod, partial.residuals=TRUE)
#'
#' # dat<- extract_partial_residuals(eff)
#'
#'
#' # library(ggplot2)
#' # library(gridExtra)
#'
#' # https://stackoverflow.com/questions/43950459/use-ggplot-to-plot-partial-effects-obtained-with-effects-library
#'
#'  # g <- ggplot(dat$fit, aes(x = education, y = fit)) +
#'  # theme_bw() +
#'  # geom_line(size = 1) +
#'  # geom_point(data = dat$residuals, aes(x = education , y =  partial.residuals), shape = 1, col = "blue", size = 2) +
#'  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
#' #  geom_smooth(data =  dat$residuals, aes(x = education, y =  partial.residuals),   method = "loess", span = 2/3, linetype = "dashed", se = FALSE)
#'
extract_partial_residuals <- function(x, ...) {
  UseMethod("extract_partial_residuals")
}


#' @rdname extract
#' @noRd
extract_partial_residuals.efflist <- function(x, ...){
  result <- list()
  cat(" model: ")
  form <- x[[1]]$formula
  attributes(form) <- NULL
  print(form)
  for (effect in names(x)){
    result[[effect]] <-  extract_partial_residuals(x[[effect]], ...)


  }
  result
}

#' @rdname extract
#' @noRd
extract_partial_residuals.eff <-
  function(x,
           term = x$term ,
           response = x$response,
    trans = NULL
    ) {

      cat(paste("\n", gsub(":", "*", x$term), 'effect\n'))
      if(is.null( trans )) trans <- I

      closest <-
        function(x, x0)
          apply(outer(
            x,
            x0,
            FUN = function(x, x0)
              abs(x - x0)
          ), 1, which.min)

      x.fit <- unlist(x$x.all)


      fit_data <-
        data.frame(
          fit = x$fit,
          lower = x$lower,
          upper = x$upper,
          term = x$x[[term]]
        )


      res_data <-
        if (is.numeric(x.fit)){
        data.frame(
          x = x.fit,
          partial.residuals = fit_data$fit[closest(trans(x.fit), fit_data$term)] + x$residuals,
          residuals = x$residuals)
        }  else{
        data.frame(
          x = x.fit,
          partial.residuals = fit_data$fit[as.numeric(factor(x.fit)) ] + x$residuals,
          residuals =  x$residuals)
          }

      res_data <- cbind(x$data, res_data)
      names(fit_data)[4] <- term

      list(fit = fit_data,
           residuals = res_data,
           partial.residuals.range=x$partial.residuals.range)
}




