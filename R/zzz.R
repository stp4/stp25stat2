
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




#' @importFrom stp25settings get_opt
#' @export
stp25settings::get_opt

#' @importFrom stp25settings set_opt
#' @export
stp25settings::set_opt

#' @importFrom stp25settings get_lang
#' @export
stp25settings::get_lang

#' @importFrom stp25settings set_lang
#' @export
stp25settings::set_lang

#' @importFrom stp25settings which_output
#' @export
stp25settings::which_output



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
