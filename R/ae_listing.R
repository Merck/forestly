# Copyright (c) 2023 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
# All rights reserved.
#
# This file is part of the forestly program.
#
# forestly is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Add inference information for AE listing analysis
#'
#' @param outdata An `outdata` object created by [prepare_ae_specific()].
#' @param display A vector with name of variable used to display on AE listing.
#'
#' @return An `outdata` object after adding AE listing information.
#'
#' @noRd
#'
#' @examples
#' adsl <- forestly_adsl
#' adae <- forestly_adae
#'
#' adsl$TRT01A <- factor(
#'   adsl$TRT01A,
#'   levels = c("Xanomeline Low Dose", "Placebo"),
#'   labels = c("Low Dose", "Placebo")
#' )
#' adae$TRTA <- factor(
#'   adae$TRTA,
#'   levels = c("Xanomeline Low Dose", "Placebo"),
#'   labels = c("Low Dose", "Placebo")
#' )
#'
#' analysis_plan <- metalite::plan(
#'   analysis = "ae_specific",
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "rel"
#' )
#'
#' meta <- metalite::meta_adam(observation = adae, population = adsl) |>
#'   metalite::define_plan(plan = analysis_plan) |>
#'   metalite::define_population(
#'     name = "apat",
#'     var = c("USUBJID", "SAFFL", "TRT01A", "SITEID", "SEX", "RACE", "AGE"),
#'     group = "TRT01A",
#'     subset = SAFFL == "Y",
#'     label = "All Participants as Treated"
#'   ) |>
#'   metalite::define_observation(
#'     name = "wk12",
#'     var = c(
#'       "USUBJID", "SAFFL", "TRTA", "SEX", "RACE", "AGE", "ASTDY",
#'       "AEDECOD", "AEBODSYS", "AESEV", "AESER", "AEREL", "AEACN",
#'       "AEOUT", "SITEID", "ADURN", "ADURU"
#'     ),
#'     group = "TRTA",
#'     subset = SAFFL == "Y",
#'     label = "Weeks 0 to 12"
#'   ) |>
#'   metalite::define_parameter(
#'     name = "rel",
#'     term1 = "Drug-Related",
#'     term2 = "",
#'     subset = AEREL %in% c("POSSIBLE", "PROBABLE"),
#'     var = "AEDECOD",
#'     soc = "AEBODSYS",
#'     label = "Drug-related AEs"
#'   ) |>
#'   metalite::define_analysis(
#'     name = "ae_specific",
#'     title = "Participants With Drug-Related Adverse Events"
#'   ) |>
#'   metalite::meta_build()
#'
#' outdata <- meta |>
#'   metalite.ae::prepare_ae_specific("apat", "wk12", "rel") |>
#'   collect_ae_listing()
#'
#' lapply(outdata, head, 10)
collect_ae_listing <- function(
    outdata,
    display = c(
      "USUBJID", "SEX", "RACE", "AGE", "ASTDY", "AESEV", "AESER",
      "AEREL", "AEACN", "AEOUT", "SITEID", "ADURN", "ADURU"
    )) {
  obs_group <- metalite::collect_adam_mapping(outdata$meta, outdata$observation)$group
  par_var <- metalite::collect_adam_mapping(outdata$meta, outdata$parameter)$var
  par_var_soc <- metalite::collect_adam_mapping(outdata$meta, outdata$parameter)$soc

  obs <- metalite::collect_observation_record(
    outdata$meta,
    outdata$population,
    outdata$observation,
    outdata$parameter,
    var = c(par_var, par_var_soc, obs_group, display)
  )

  # Keep variable used to display only
  keep <- c(par_var, par_var_soc, obs_group)
  outdata$ae_listing <- obs[, c(keep, setdiff(display, keep))]

  # Rename the fixed columns
  names(outdata$ae_listing)[1:3] <- c(
    "Adverse_Event",
    "SOC_Name",
    "Treatment_Group"
  )

  # Get all labels from the un-subset data
  listing_label <- get_label(obs)
  # Assign labels
  outdata$ae_listing <- assign_label(
    data = outdata$ae_listing,
    var = names(outdata$ae_listing),
    label = listing_label[match(names(outdata$ae_listing), names(listing_label))]
  )

  outdata
}

#' Convert character strings or factor to proper case
#'
#' This function converts the first letter of each string to uppercase and the rest to lowercase.
#' It handles both character vectors and factors robustly.
#'
#' @param x A character vector or factor.
#'
#' @return A character vector with proper case applied.
#'
#' @noRd
#'
#' @examples
#' propercase("AbCDe FGha")
#' propercase(factor(c("MILD", "MODERATE", "SEVERE")))
propercase <- function(x) {
  if (is.factor(x)) {
    # For factors, apply proper case to levels to preserve factor structure
    levels(x) <- paste0(toupper(substr(levels(x), 1, 1)), tolower(substring(levels(x), 2)))
    # Return as character to match expected output type
    return(as.character(x))
  } else if (is.character(x)) {
    # For character vectors, apply proper case directly
    return(paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))))
  } else {
    # For other types, convert to character first then apply proper case
    x_char <- as.character(x)
    return(paste0(toupper(substr(x_char, 1, 1)), tolower(substring(x_char, 2))))
  }
}

#' Convert character strings or factor to title case
#'
#' Handles both character vectors and factors. For factors, it preserves the
#' factor structure by modifying only the levels, which maintains the original
#' ordering. `tools::toTitleCase()` is applied to the unique values only, which
#' keeps it fast on large columns with many repeated values (issue #129).
#'
#' @param x A character vector or factor.
#' @param lower Logical indicating whether to convert to lowercase first (default TRUE).
#'
#' @return A character vector with title case applied.
#'
#' @noRd
#'
#' @examples
#' titlecase(c("american indian or alaska native", "WHITE")) # char vector
#' titlecase(factor(c("tHEre is oNe", "tHAt is tWo", "heRe is tHRee"))) # factor
#' titlecase(c("F", "M")) # char vector
#' titlecase(factor(c("F", "M"))) # factor
titlecase <- function(x, lower = TRUE) {
  # Title-case the distinct values once, then map back onto the full vector.
  convert <- function(text) {
    if (lower) text <- tolower(text)
    u <- unique(text)
    tools::toTitleCase(u)[match(text, u)]
  }

  if (is.factor(x)) {
    # Operate on levels to preserve factor structure, then return as character
    # to match the expected output type.
    levels(x) <- convert(levels(x))
    as.character(x)
  } else {
    convert(as.character(x))
  }
}

#' Format AE listing analysis
#'
#' @param outdata An `outdata` object created by [prepare_ae_specific()].
#' @param ae_listing_labels  A vector with label of variable used to display on AE listing.
#' @param display_unique_records A logical value to display only unique records
#'   on AE listing table.
#'
#' @return An `outdata` object after adding AE listing information.
#'
#' @noRd
#'
#' @examples
#' adsl <- forestly_adsl
#' adae <- forestly_adae
#'
#' adsl$TRT01A <- factor(
#'   adsl$TRT01A,
#'   levels = c("Xanomeline Low Dose", "Placebo"),
#'   labels = c("Low Dose", "Placebo")
#' )
#' adae$TRTA <- factor(
#'   adae$TRTA,
#'   levels = c("Xanomeline Low Dose", "Placebo"),
#'   labels = c("Low Dose", "Placebo")
#' )
#'
#' analysis_plan <- metalite::plan(
#'   analysis = "ae_specific",
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "rel"
#' )
#'
#' meta <- metalite::meta_adam(observation = adae, population = adsl) |>
#'   metalite::define_plan(plan = analysis_plan) |>
#'   metalite::define_population(
#'     name = "apat",
#'     var = c("USUBJID", "SAFFL", "TRT01A", "SITEID", "SEX", "RACE", "AGE"),
#'     group = "TRT01A",
#'     subset = SAFFL == "Y",
#'     label = "All Participants as Treated"
#'   ) |>
#'   metalite::define_observation(
#'     name = "wk12",
#'     var = c(
#'       "USUBJID", "SAFFL", "TRTA", "SEX", "RACE", "AGE", "ASTDY",
#'       "AEDECOD", "AEBODSYS", "AESEV", "AESER", "AEREL", "AEACN",
#'       "AEOUT", "SITEID", "ADURN", "ADURU", "AOCCPFL"
#'     ),
#'     group = "TRTA",
#'     subset = SAFFL == "Y",
#'     label = "Weeks 0 to 12"
#'   ) |>
#'   metalite::define_parameter(
#'     name = "rel",
#'     term1 = "Drug-Related",
#'     term2 = "",
#'     subset = AEREL %in% c("POSSIBLE", "PROBABLE"),
#'     var = "AEDECOD",
#'     soc = "AEBODSYS",
#'     label = "Drug-related AEs"
#'   ) |>
#'   metalite::define_analysis(
#'     name = "ae_specific",
#'     title = "Participants With Drug-Related Adverse Events"
#'   ) |>
#'   metalite::meta_build()
#'
#' outdata <- metalite.ae::prepare_ae_specific(meta, "apat", "wk12", "rel") |>
#'   collect_ae_listing(
#'     c(
#'       "USUBJID", "SEX", "RACE", "AGE", "ASTDY", "AESEV", "AESER",
#'       "AEREL", "AEACN", "AEOUT", "SITEID", "ADURN", "ADURU", "AOCCPFL"
#'     )
#'   ) |>
#'   format_ae_listing()
#'
#' lapply(outdata, head, 20)
format_ae_listing <- function(outdata, ae_listing_labels = NULL, display_unique_records = FALSE) {
  res <- outdata[["ae_listing"]]
  obs_group <- metalite::collect_adam_mapping(outdata$meta, outdata$observation)$group
  par_var <- metalite::collect_adam_mapping(outdata$meta, outdata$parameter)$var
  par_var_soc <- metalite::collect_adam_mapping(outdata$meta, outdata$parameter)$soc

  # Customized variable will use label as column header in
  # drill down listing on interactive forest plot
  res_columns <- names(res)
  if (!display_unique_records) {
    outdata[["ae_listing"]] <- res[, res_columns]
  } else {
    outdata[["ae_listing"]] <- unique(res[, res_columns])
  }

  if (!is.null(ae_listing_labels)) {
    listing_label <- c("Adverse Event", "SOC Name", "Treatment Group", ae_listing_labels)
    names(listing_label) <- names(res)
  } else {
    # Get all labels from the un-subset data
    listing_label <- get_label(res)
  }
  # Assign labels
  outdata[["ae_listing"]] <- assign_label(
    data = outdata[["ae_listing"]],
    var = names(outdata[["ae_listing"]]),
    label = listing_label[match(names(outdata[["ae_listing"]]), names(listing_label))]
  )

  outdata
}
