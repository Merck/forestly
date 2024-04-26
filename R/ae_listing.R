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
#' meta <- metalite.ae::meta_ae_example()
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

  obs <- metalite::collect_observation_record(
    outdata$meta,
    outdata$population,
    outdata$observation,
    outdata$parameter,
    var = c(par_var, obs_group, display)
  )

  # Keep variable used to display only
  outdata$ae_listing <- obs[, c(par_var, obs_group, display)]

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

#' Convert character strings to proper case
#'
#' @param x A character vector.
#'
#' @return A character vector.
#'
#' @noRd
#'
#' @examples
#' propercase("AbCDe FGha")
propercase <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

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
#' meta <- metalite.ae::meta_ae_example()
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
  res <- outdata$ae_listing

  obs_group <- metalite::collect_adam_mapping(outdata$meta, outdata$observation)$group
  par_var <- metalite::collect_adam_mapping(outdata$meta, outdata$parameter)$var

  new_name <- c("SITEID", "SITENUM", "USUBJID", "SUBJID", "SEX", "RACE", "AGE", obs_group, "EPOCH",
                "ASTDY", par_var, "ADURN", "AESEV", "AESER", "AEREL", "AREL", "AEACN",
                "AEOUT", "AEDOSDUR")
  name_mapping <- c("Site_Number", "Site_Number", "Participant_ID", "Participant_ID", "Gender", "Race", "Age", "Treatment_Group", "Onset_Epoch",
                    "Relative_Day_of_Onset", "Adverse_Event", "Duration", "Intensity", "Serious", "Related", "Related", "Action_Taken",
                    "Outcome", "Total_Dose_on_Day_of_AE_Onset")
  names(name_mapping) <- new_name

  res_columns <- lapply(names(res), function(x) {
    if (toupper(x) %in% names(name_mapping)) {name_mapping[[toupper(x)]]}
    else {x}
  }) |> unlist()

  # Site ID
  if ("SITEID" %in% toupper(names(res))) {
    res$Site_Number <- propercase(res$SITEID)
  }

  if ("SITENUM" %in% toupper(names(res))) {
    res$Site_Number <- res$SITENUM
  }

  # Participant ID
  if ("USUBJID" %in% toupper(names(res))) {
    res$Participant_ID <- res$USUBJID
  }
  if ("SUBJID" %in% toupper(names(res))) {
    res$Participant_ID <- res$SUBJID
  }
  attr(res$Participant_ID, "label") <- NULL

  res$Gender <- tools::toTitleCase(res$SEX)

  res$Race <- tools::toTitleCase(tolower(res$RACE))

  res$Age <- res$AGE
  res$Treatment_Group <- res[[obs_group]]
  attr(res$Treatment_Group, "label") <- NULL

  # Onset epoch
  if ("EPOCH" %in% toupper(names(res))) {
    res$Onset_Epoch <- tools::toTitleCase(tolower(res$EPOCH)) # propcase the EPOCH
  }

  # Relative day of onset (ASTDY)
  if ("ASTDY" %in% toupper(names(res))) {
    res$Relative_Day_of_Onset <- res$ASTDY
  }

  # Adverse event
  res$Adverse_Event <- propercase(res[[par_var]])
  res <- res[, !(names(res) == par_var)]

  # Duration
  if ("ADURN" %in% toupper(names(res)) & "ADURU" %in% toupper(names(res))) {
    res$Duration <- paste(ifelse(is.na(res$ADURN), "", as.character(res$ADURN)), tools::toTitleCase(tolower(res$ADURU)), sep = " ") # AE duration with unit

    if (length(res$Duration > 0)){
      for (i in 1:length(res$Duration)) {
        if (is.na(res$ADURN[i])) {
          res$Duration[i] <- ifelse(charmatch(toupper(res$AEOUT[i]), "RECOVERING/RESOLVING") > 0 |
            charmatch(toupper(res$AEOUT[i]), "NOT RECOVERED/NOT RESOLVED") > 0, "Continuing", "Unknown")
        }
      }
    }
  }

  # Intensity
  if ("AESEV" %in% toupper(names(res))) {
    res$Intensity <- propercase(res$AESEV)
  }

  # Serious
  if ("AESER" %in% toupper(names(res))) {
    res$Serious <- propercase(res$AESER)
  }

  # AE related
  if ("AEREL" %in% toupper(names(res))) {
    res$Related <- ifelse(res$AEREL == "RELATED", "Y", ifelse(
      toupper(res$AEREL) == "NOT RELATED", "N", tools::toTitleCase(tolower(res$AEREL))
    ))
  }

  if ("AREL" %in% toupper(names(res))) {
    res$Related <- ifelse(res$AREL == "RELATED", "Y", ifelse(
      toupper(res$AREL) == "NOT RELATED", "N", tools::toTitleCase(tolower(res$AREL))
    ))
  }

  # Action taken
  if ("AEACN" %in% toupper(names(res))) {
    if (length(res$AEACN > 0)) {
      for (i in 1:length(res$AEACN)) {
        res$Action_Taken[i] <- switch(res$AEACN[i],
          "DOSE NOT CHANGED" = "None",
          "DOSE REDUCED" = "Reduced",
          "DRUG INTERRUPTED" = "Interrupted",
          "DOSE INCREASED" = "Increased",
          "NOT APPLICABLE" = "N/A",
          "UNKNOWN" = "Unknown",
          tools::toTitleCase(tolower(res$AEACN[i]))
        )
      }
    } else {
      res$Action_Taken <- res$AEACN
    }
  }

  # Outcome
  if ("AEOUT" %in% toupper(names(res))) {
    if (length(res$AEOUT > 0)) {
      for (i in 1:length(res$AEOUT)) {
        res$Outcome[i] <- switch(res$AEOUT[i],
          "RECOVERED/RESOLVED" = "Resolved",
          "RECOVERING/RESOLVING" = "Resolving",
          "RECOVERED/RESOLVED WITH SEQUELAE" = "Sequelae",
          "NOT RECOVERED/NOT RESOLVED" = "Not Resolved",
          tools::toTitleCase(tolower(res$AEOUT[i]))
        )
      }
    } else {
      res$Outcome <- res$AEOUT
    }
  }

  # Total dose on day of AE onset
  if ("AEDOSDUR" %in% toupper(names(res))) {
    res$ymd <- substring(res$AEDOSDUR, unlist(gregexpr("/P", res$AEDOSDUR)) + 2)

    res$Total_Dose_on_Day_of_AE_Onset <- ""
    print(length(res$AEDOSDUR))
    print(nrow(res$AEDOSDUR))

    if (length(res$AEDOSDUR) > 0){
      for (i in 1:length(res$AEDOSDUR)) {
        if (unlist(gregexpr("Y", res$ymd[i])) > 0) {
          val_year <- substring(res$ymd[i], 1, unlist(gregexpr("Y", res$ymd[i])) - 1)
          if (as.numeric(val_year) != 1) {
            res$Total_Dose_on_Day_of_AE_Onset[i] <- paste0(res$Total_Dose_on_Day_of_AE_Onset[i], val_year, " years")
          } else {
            res$Total_Dose_on_Day_of_AE_Onset[i] <- paste0(res$Total_Dose_on_Day_of_AE_Onset[i], "1 year")
          }

          res$ymd[i] <- substring(res$ymd[i], unlist(gregexpr("Y", res$ymd[i])) + 1)
        }
        if (unlist(gregexpr("M", res$ymd[i])) > 0) {
          val_month <- substring(res$ymd[i], 1, unlist(gregexpr("M", res$ymd[i])) - 1)

          if (as.numeric(val_month) != 1) {
            res$Total_Dose_on_Day_of_AE_Onset[i] <- paste0(res$Total_Dose_on_Day_of_AE_Onset[i], " ", val_month, " months")
          } else {
            res$Total_Dose_on_Day_of_AE_Onset[i] <- paste0(res$Total_Dose_on_Day_of_AE_Onset[i], " 1 month")
          }

          res$ymd[i] <- substring(res$ymd[i], unlist(gregexpr("M", res$ymd[i])) + 1)
        }
        if (unlist(gregexpr("D", res$ymd[i])) > 0) {
          val_day <- substring(res$ymd[i], 1, unlist(gregexpr("D", res$ymd[i])) - 1)

          if (as.numeric(val_day) != 1) {
            res$Total_Dose_on_Day_of_AE_Onset[i] <- paste0(res$Total_Dose_on_Day_of_AE_Onset[i], " ", val_day, " days")
          } else {
            res$Total_Dose_on_Day_of_AE_Onset[i] <- paste0(res$Total_Dose_on_Day_of_AE_Onset[i], " 1 day")
          }
        }
      }
    } else {
      res$Total_Dose_on_Day_of_AE_Onset <- res$AEDOSDUR
    }
    res <- res[, !(names(res) == "ymd"), drop = FALSE]
  }

  # Customized variable will use label as column header in
  # drill down listing on interactive forest plot
  if (!display_unique_records) {
    outdata$ae_listing <- res[, res_columns]
  } else {
    outdata$ae_listing <- res[, res_columns] |> unique()
  }

  # Get all labels from the un-subset data
  listing_label <- get_label(res)
  listing_label <- gsub("_", " ", listing_label)
  # Assign labels
  outdata$ae_listing <- assign_label(
    data = outdata$ae_listing,
    var = names(outdata$ae_listing),
    label = listing_label[match(names(outdata$ae_listing), names(listing_label))]
  )

  outdata
}
