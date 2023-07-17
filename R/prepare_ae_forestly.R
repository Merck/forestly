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

#' Prepare datasets for interactive forest plot
#'
#' @inheritParams metalite.ae::prepare_ae_specific
#' @param ae_listing_display A vector of name of variables used to display
#'   on AE listing table.
#'
#' @return An `outdata` object.
#'
#' @export
#'
#' @examples
#' adsl <- forestly_adsl[1:100,]
#' adae <- forestly_adae[1:100,]
#' meta_forestly(
#'   dataset_adsl = adsl,
#'   dataset_adae = adae,
#'   population_term = "apat",
#'   observation_term = "wk12"
#' ) |>
#'   prepare_ae_forestly(parameter = "any;rel")
prepare_ae_forestly <- function(
    meta,
    population = NULL,
    observation = NULL,
    parameter,
    reference_group = NULL,
    ae_listing_display = c(
      "SEX", "RACE", "AGE", "ASTDY", "AESEV", "AESER",
      "AEREL", "AEACN", "AEOUT", "SITEID", "ADURN", "ADURU"
    )) {
  parameters <- unlist(strsplit(parameter, ";"))

  if (is.null(population)) {
    if (length(meta$population) == 1) {
      population <- meta$population[[1]]$name
    } else {
      stop("Population term should be one selected from metadata.")
    }
  }

  if (is.null(observation)) {
    if (length(meta$observation) == 1) {
      observation <- meta$observation[[1]]$name
    } else {
      stop("Observation term should be one selected from metadata.")
    }
  }

  res <- lapply(parameters, function(x) {
    # print(x)
    metalite.ae::prepare_ae_specific(meta,
      population = population, observation = observation,
      parameter = x,
      components = "par",
      reference_group = reference_group
    ) |>
      metalite.ae::extend_ae_specific_inference() |>
      collect_ae_listing(display = ae_listing_display) |>
      format_ae_listing()
  })

  ae_listing <- data.frame()
  for (i in 1:length(res)) {
    res[[i]]$ae_listing$param <- res[[i]]$parameter
    ae_listing <- rbind(ae_listing, res[[i]]$ae_listing)
  }

  # Arrange data frame
  foo <- function(name) {
    tmp <- lapply(res, function(x) {
      x0 <- data.frame(x[[name]][x[["order"]] >= 1000, ])
      names(x0) <- names(x[[name]])
      x0
    })
    do.call(rbind, tmp)
  }

  name <- c("n", "prop", "diff", "ci_lower", "ci_upper", "p")
  values <- lapply(name, foo)
  names(values) <- name

  # Arrange vector
  foo <- function(name) {
    tmp <- lapply(res, function(x) {
      x[[name]][x[["order"]] >= 1000]
    })
    n <- vapply(tmp, length, FUN.VALUE = numeric(1))
    tmp <- unlist(tmp)
    attr(tmp, "n") <- n
    tmp
  }

  name <- c("order", "name")
  info <- lapply(name, foo)
  names(info) <- name
  parameter_order <- unlist(Map(rep, x = parameters, each = attributes(info$order)$n))
  names(parameter_order) <- NULL
  parameter_order <- factor(parameter_order, levels = parameters)

  # Additional group information
  info1 <- do.call(data.frame, info)

  # Prepare outdata
  metalite::outdata(
    meta = meta,
    population = population,
    observation = observation,
    parameter = parameter,
    n = values$n,
    order = info$order,
    parameter_order = parameter_order,
    group = res[[1]]$group,
    reference_group = res[[1]]$reference_group,
    prop = values$prop,
    diff = values$diff,
    n_pop = res[[1]]$n_pop,
    name = info$name,
    ci_lower = values$ci_lower,
    ci_upper = values$ci_upper,
    p = values$p,
    ae_listing = ae_listing
  )
}
