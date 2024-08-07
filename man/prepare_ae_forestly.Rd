% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_ae_forestly.R
\name{prepare_ae_forestly}
\alias{prepare_ae_forestly}
\title{Prepare datasets for interactive forest plot}
\usage{
prepare_ae_forestly(
  meta,
  population = NULL,
  observation = NULL,
  parameter = NULL,
  components = "par",
  reference_group = NULL,
  ae_listing_display = c("USUBJID", "SITEID", "SEX", "RACE", "AGE", "ASTDY", "AESER",
    "AEREL", "AEACN", "AEOUT", "ADURN", "ADURU"),
  ae_listing_unique = FALSE
)
}
\arguments{
\item{meta}{A metadata object created by metalite.}

\item{population}{A character value of population term name.
The term name is used as key to link information.}

\item{observation}{A character value of observation term name.
The term name is used as key to link information.}

\item{parameter}{A character value of parameter term name.
The term name is used as key to link information.}

\item{components}{A character vector of components name.}

\item{reference_group}{An integer to indicate reference group.
Default is 2 if there are 2 groups, otherwise, the default is 1.}

\item{ae_listing_display}{A vector of name of variables used to display
on AE listing table.}

\item{ae_listing_unique}{A logical value to display only unique records
on AE listing table.}
}
\value{
An \code{outdata} object.
}
\description{
Prepare datasets for interactive forest plot
}
\examples{
adsl <- forestly_adsl[1:100,]
adae <- forestly_adae[1:100,]
meta_forestly(
  dataset_adsl = adsl,
  dataset_adae = adae
) |>
  prepare_ae_forestly()
}
