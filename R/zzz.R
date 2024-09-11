.onAttach <- function(...) {

  oldc <- getOption("contrasts")
  contrasts =  c("contr.Treatment", "contr.poly")
  options(contrasts = contrasts)

  packageStartupMessage(
    "I changed the contrasts from",
    paste(oldc, collapse =  ),
    "\nto ", paste(contrasts, collapse =  ),
    "!\n\n")


}




# Information ueber die verwendeten Methoden
#
# @param methode,library,fun  Text
#
# @return data.frame()
Info_Statistic <-
  function(methode = "describe",
           library = "base",
           fun = "summary",
           my_methodes = "") {
    cat("Methodes: ",  my_methodes)
    data.frame(
      Methode = methode,
      Library = library,
      Function = fun,
      stringsAsFactors = FALSE
    )
  }



#' Analytics
#'
#' Collection of convenient functions for common statistical computations.
#'
#' @md
#' @name stp25stat2
#' @docType package
#' @author Wolfgang Peter (w.peter@@statistik-peter.at)
"_PACKAGE"

#' @importFrom methods is

#' @importFrom stats AIC BIC IQR addmargins aggregate as.formula binomial coef
#'  confint cor deviance df drop1 family fisher.test fitted formula ftable glm
#'  lm logLik median model.frame model.matrix na.omit na.pass pairwise.t.test
#'  pairwise.wilcox.test pchisq pf pnorm qnorm quantile reformulate sd sigma
#'  summary.aov update var vcov



#' @importFrom utils capture.output data head methods str

#' @importFrom stp25settings get_opt
#' @export
stp25settings::get_opt

#' @importFrom stp25settings set_opt
#' @export
stp25settings::set_opt

#' @importFrom stp25settings which_output
#' @export
stp25settings::which_output

#' @importFrom stp25settings farbe
#' @export
stp25settings::farbe

#' @importFrom stp25settings standard_theme
#' @export
stp25settings::standard_theme

#' @importFrom stp25settings ggplot_theme
#' @export
stp25settings::ggplot_theme

#' @importFrom stp25settings bw_theme
#' @export
stp25settings::bw_theme

#' @importFrom stp25tools wrap_string
#' @export
stp25tools::wrap_string

# wird won stp25tools explortiert
# @importFrom Hmisc Cs
# @export
# Hmisc::Cs

#' @importFrom car contr.Treatment
#' @export
car::contr.Treatment
#' @importFrom car contr.Sum
#' @export
#'
car::contr.Sum
#' @importFrom car contr.Helmert
#' @export
car::contr.Helmert
