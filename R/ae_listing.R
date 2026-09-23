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
  outdata$ae_listing <- obs[, c(par_var, par_var_soc, obs_group, display)]

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

#' Convert character strings or factor to title case (optimized with memoization)
#'
#' This function provides a fast implementation of title case conversion that handles
#' both character vectors and factors. For factors, it preserves the factor structure
#' by modifying only the levels, which maintains the original ordering.
#' This optimized version uses memoization to cache converted values, dramatically
#' speeding up performance for large datasets with repeated values.
#'
#' Based on Yihui Xie's profiling and optimization approach for issue #129.
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
titlecase <- local({
  # Memoized version of tools::toTitleCase with preprocessing
  # Maintains separate caches for lower=TRUE and lower=FALSE cases
  cache_lower_true <- list()
  cache_lower_false <- list()

  function(x, lower = TRUE) {
    # Helper function to apply memoized title case
    memoized_titlecase <- function(text, to_lower = TRUE) {
      if (length(text) == 0) return(character(0))

      # Choose appropriate cache
      cache <- if (to_lower) cache_lower_true else cache_lower_false

      # Preprocess text if needed
      if (to_lower) {
        text <- tolower(text)
      }

      # Find values not yet in cache
      not_cached <- !text %in% names(cache)

      if (any(not_cached)) {
        # Get unique values that need conversion
        unique_new <- unique(text[not_cached])

        # Convert using tools::toTitleCase and update cache
        converted <- tools::toTitleCase(unique_new)
        names(converted) <- unique_new

        # Update the appropriate cache
        if (to_lower) {
          cache_lower_true <<- c(cache_lower_true, as.list(converted))
        } else {
          cache_lower_false <<- c(cache_lower_false, as.list(converted))
        }

        # Update local cache reference
        cache <- if (to_lower) cache_lower_true else cache_lower_false
      }

      # Return cached results
      unlist(cache[text], use.names = FALSE)
    }

    if (is.factor(x)) {
      # For factors, apply title case to levels to preserve factor structure
      levels(x) <- memoized_titlecase(levels(x), to_lower = lower)
      # Return as character to match expected output type
      return(as.character(x))
    } else {
      # For character vectors and other types, convert to character and apply title case
      x_char <- as.character(x)
      return(memoized_titlecase(x_char, to_lower = lower))
    }
  }
})

#' Format AE listing analysis
#'
#' @param outdata An `outdata` object created by [prepare_ae_specific()].
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
format_ae_listing <- function(outdata, display_unique_records = FALSE) {
  res <- outdata[["ae_listing"]]
  obs_group <- metalite::collect_adam_mapping(outdata$meta, outdata$observation)$group
  par_var <- metalite::collect_adam_mapping(outdata$meta, outdata$parameter)$var
  par_var_soc <- metalite::collect_adam_mapping(outdata$meta, outdata$parameter)$soc

  new_name <- c(
    "SITEID", "SITENUM", "USUBJID", "SUBJID", "SEX", "RACE", "AGE", obs_group, "EPOCH",
    "ASTDY", par_var, par_var_soc, "ADURN", "AESEV", "AESER", "AEREL", "AREL", "AEACN",
    "AEOUT", "AEDOSDUR", "ATOXGRN"
  )
  name_mapping <- c(
    "Site_Number", "Site_Number", "Unique_Participant_ID", "Participant_ID", "Gender", "Race", "Age", "Treatment_Group", "Onset_Epoch",
    "Relative_Day_of_Onset", "Adverse_Event", "SOC_Name", "Duration", "Intensity", "Serious", "Related", "Related", "Action_Taken",
    "Outcome", "Total_Dose_on_Day_of_AE_Onset", "Maximum_Toxicity_Grade"
  )
  names(name_mapping) <- new_name

  res_columns <- lapply(names(res), function(x) {
    if (toupper(x) %in% names(name_mapping)) {
      name_mapping[[toupper(x)]]
    } else {
      x
    }
  }) |> unlist()

  # Site ID
  if ("SITEID" %in% toupper(names(res))) {
    res[["Site_Number"]] <- propercase(res[["SITEID"]])
  }

  if ("SITENUM" %in% toupper(names(res))) {
    res[["Site_Number"]] <- res[["SITENUM"]]
  }

  # Participant ID
  if ("USUBJID" %in% toupper(names(res))) {
    res[["Unique_Participant_ID"]] <- res[["USUBJID"]]
  }
  if ("SUBJID" %in% toupper(names(res))) {
    res[["Participant_ID"]] <- res[["SUBJID"]]
  }
  attr(res[["Participant_ID"]], "label") <- NULL

  if ("SEX" %in% toupper(names(res))) {
    res[["Gender"]] <- titlecase(res[["SEX"]], lower = FALSE)
  }

  if ("RACE" %in% toupper(names(res))) {
    res[["Race"]] <- titlecase(res[["RACE"]], lower = TRUE)
  }

  if ("AGE" %in% toupper(names(res))) {
    res[["Age"]] <- res[["AGE"]]
  }

  res[["Treatment_Group"]] <- res[[obs_group]]

  attr(res[["Treatment_Group"]], "label") <- NULL

  # Onset epoch
  if ("EPOCH" %in% toupper(names(res))) {
    res[["Onset_Epoch"]] <- titlecase(res[["EPOCH"]], lower = TRUE) # propcase the EPOCH
  }

  # Relative day of onset (ASTDY)
  if ("ASTDY" %in% toupper(names(res))) {
    res[["Relative_Day_of_Onset"]] <- res[["ASTDY"]]
  }

  # SOC
  res[["SOC_Name"]] <- res[[par_var_soc]]

  # Adverse event
  res[["Adverse_Event"]] <- propercase(res[[par_var]])
  res <- res[, !(names(res) == par_var)]

  # Duration
  if ("ADURN" %in% toupper(names(res)) & "ADURU" %in% toupper(names(res))) {
    res[["Duration"]] <- paste(ifelse(is.na(res[["ADURN"]]), "", as.character(res[["ADURN"]])),
      titlecase(res[["ADURU"]], lower = TRUE),
      sep = " "
    ) # AE duration with unit

    na_dur <- is.na(res[["ADURN"]])
    if (any(na_dur)) {
      aeout_na <- toupper(res[["AEOUT"]][na_dur])
      res[["Duration"]][na_dur] <- ifelse(
        aeout_na %in% c("RECOVERING/RESOLVING", "NOT RECOVERED/NOT RESOLVED"),
        "Continuing", "Unknown"
      )
    }
    res <- res[, !(names(res) %in% "ADURU")]
    res_columns <- res_columns[!(res_columns %in% "ADURU")]
  }

  # Intensity
  if ("AESEV" %in% toupper(names(res))) {
    res[["Intensity"]] <- propercase(res[["AESEV"]])
  }

  # Maximum toxicity grade
  if ("ATOXGRN" %in% toupper(names(res))) {
    res[["Maximum_Toxicity_Grade"]] <- res[["ATOXGRN"]]
  }

  # Serious
  if ("AESER" %in% toupper(names(res))) {
    res[["Serious"]] <- propercase(res[["AESER"]])
  }

  # AE related
  if ("AEREL" %in% toupper(names(res))) {
    res[["Related"]] <- ifelse(res[["AEREL"]] == "RELATED", "Y", ifelse(
      toupper(res[["AEREL"]]) == "NOT RELATED", "N", titlecase(res[["AEREL"]], lower = TRUE)
    ))
  }

  # Action taken
  if ("AEACN" %in% toupper(names(res))) {
    acn <- res[["AEACN"]]
    acn_map <- c(
      "DOSE NOT CHANGED" = "None",
      "DOSE REDUCED" = "Reduced",
      "DRUG INTERRUPTED" = "Interrupted",
      "DOSE INCREASED" = "Increased",
      "NOT APPLICABLE" = "N/A",
      "UNKNOWN" = "Unknown"
    )
    mapped <- acn_map[acn]
    # Values not in the lookup fall back to title case of the original value.
    res[["Action_Taken"]] <- ifelse(is.na(mapped), titlecase(acn, lower = TRUE), unname(mapped))
  }

  # Outcome
  if ("AEOUT" %in% toupper(names(res))) {
    out <- res[["AEOUT"]]
    out_map <- c(
      "RECOVERED/RESOLVED" = "Resolved",
      "RECOVERING/RESOLVING" = "Resolving",
      "RECOVERED/RESOLVED WITH SEQUELAE" = "Sequelae",
      "NOT RECOVERED/NOT RESOLVED" = "Not Resolved"
    )
    mapped <- out_map[out]
    # Values not in the lookup fall back to title case of the original value.
    res[["Outcome"]] <- ifelse(is.na(mapped), titlecase(out, lower = TRUE), unname(mapped))
  }
  # Total dose on day of AE onset
  if ("AEDOSDUR" %in% toupper(names(res))) {
    # Parse the ISO-8601-style duration (e.g. ".../P1Y2M3D") into a human
    # readable string such as "1 year 2 months 3 days". This is vectorized: for
    # each Y/M/D designator we pull the digits immediately preceding it (or NA
    # when the designator is absent) and format that one component. See #160 for
    # the previous per-row loop, which errored when a designator repeated.
    ymd <- sub(".*?/P", "", res[["AEDOSDUR"]])

    # Digits directly before `letter`, or NA when the designator is absent.
    extract_unit <- function(s, letter) {
      val <- rep(NA_character_, length(s))
      has <- grepl(letter, s, fixed = TRUE)
      val[has] <- sub(paste0("^.*?([0-9]+)", letter, ".*$"), "\\1", s[has])
      val
    }

    # One formatted component ("", "1 year", "2 years", ...). `lead` is the
    # separator placed before the component when it is present; the year
    # component carries no leading space, months and days each carry one, which
    # reproduces the spacing of the original implementation.
    format_unit <- function(val, singular, plural, lead) {
      n <- suppressWarnings(as.numeric(val))
      content <- ifelse(
        n == 1,
        paste0("1 ", singular),
        paste0(val, " ", plural)
      )
      ifelse(is.na(val), "", paste0(lead, content))
    }

    res[["Total_Dose_on_Day_of_AE_Onset"]] <- paste0(
      format_unit(extract_unit(ymd, "Y"), "year", "years", ""),
      format_unit(extract_unit(ymd, "M"), "month", "months", " "),
      format_unit(extract_unit(ymd, "D"), "day", "days", " ")
    )
  }


  # Customized variable will use label as column header in
  # drill down listing on interactive forest plot
  if (!display_unique_records) {
    outdata[["ae_listing"]] <- res[, res_columns]
  } else {
    outdata[["ae_listing"]] <- unique(res[, res_columns])
  }

  # Get all labels from the un-subset data
  listing_label <- get_label(res)
  listing_label <- gsub("_", " ", listing_label)
  # Assign labels
  outdata[["ae_listing"]] <- assign_label(
    data = outdata[["ae_listing"]],
    var = names(outdata[["ae_listing"]]),
    label = listing_label[match(names(outdata[["ae_listing"]]), names(listing_label))]
  )

  outdata
}
