% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_output.R
\name{prepare_output}
\alias{prepare_output}
\title{prepare_output}
\usage{
prepare_output(
  x,
  caption = "",
  note = "",
  N = NULL,
  labels = NA,
  rgroup = NULL,
  n.rgroup = NULL,
  ...
)
}
\arguments{
\item{x}{data.frame Objekt Liste oder df}

\item{caption}{Uberschrift}

\item{note}{Anmerkung}

\item{N, include.n}{Anzahl}

\item{labels}{Label}

\item{rgroup, n.rgroup}{an htmlTable {htmlTable}}

\item{...}{Weitere Argumente}
}
\value{
tibble mit attributen
}
\description{
Ergebnis fuer Output vorbereiten
}
