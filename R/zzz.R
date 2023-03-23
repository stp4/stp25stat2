
.onLoad <- function(libname, pkgname) {
  # oldc <- getOption("contrasts")
  # contrasts =  c("contr.Treatment", "contr.poly")
  # options(contrasts = contrasts)
  packageStartupMessage("\nHallo Wolfgang!\n\n")
}

.onAttach <- function(...) {

  oldc <- getOption("contrasts")
  contrasts =  c("contr.Treatment", "contr.poly")
  options(contrasts = contrasts)

  packageStartupMessage(
    "Die Kontraste habe ich auf von ",
    paste(oldc, collapse = ", "),
    "\nauf ", paste(contrasts, collapse = ", "),
    " umgestellt!\n\n")


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




#' @importFrom stp25settings get_opt
#' @export
stp25settings::get_opt

#' @importFrom stp25settings set_opt
#' @export
stp25settings::set_opt

# @importFrom stp25settings get_lang
# @export
#stp25settings::get_lang

# @importFrom stp25settings set_lang
# @export
#stp25settings::set_lang

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

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom Hmisc Cs
#' @export
Hmisc::Cs

#' @importFrom car contr.Treatment
#' @export
car::contr.Treatment
#' @importFrom car contr.Sum
#' @export
car::contr.Sum
#' @importFrom car contr.Helmert
#' @export
car::contr.Helmert








#' Information ueber die verwendeten Methoden
#'
#' @param methode,library,fun  Text
#'
#' @return data.frame()

Info_Statistic <-
  function(methode = "describe",
           library = "base",
           fun = "summary",
           my_methodes = "") {
    Text("Methodes: ",  my_methodes)
    data.frame(
      Methode = methode,
      Library = library,
      Function = fun,
      stringsAsFactors = FALSE
    )
  }
