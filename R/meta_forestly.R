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

#' Create metadata for interactive forest plot
#'
#' @param dataset_adsl ADSL source dataset.
#' @param dataset_adae ADAE source dataset.
#' @param population_term A character value of population term name.
#' @param observation_term A character value of observation term name.
#' @param population_subset An unquoted condition for selecting the
#'   populations from ADSL dataset.
#' @param observation_subset An unquoted condition for selecting the
#'   observations from ADAE dataset.
#' @param treatment_group A character value of treatment group name.
#' @param parameter_term A character value of parameter term name.
#'
#' @return A metalite object.
#'
#' @export
#'
#' @examples
#' meta_forestly(
#'   forestly_adsl,
#'   forestly_adae,
#'   population_term = "apat",
#'   observation_term = "wk12",
#'   parameter_term = "any;rel"
#' )
meta_forestly <- function(
    dataset_adsl,
    dataset_adae,
    population_term,
    observation_term,
    parameter_term = "any;rel",
    population_subset = SAFFL == "Y",
    observation_subset = SAFFL == "Y",
    treatment_group = "TRTA") {
  meta <- metalite::meta_adam(
    population = as.data.frame(dataset_adsl),
    observation = as.data.frame(dataset_adae)
  ) |>
    metalite::define_plan(plan = metalite::plan(
      analysis = "ae_forestly",
      population = population_term,
      observation = observation_term,
      parameter = parameter_term
    )) |>
    metalite::define_population(
      name = population_term,
      group = treatment_group,
      subset = !!rlang::enquo(population_subset),
      label = ""
    ) |>
    metalite::define_observation(
      name = observation_term,
      group = treatment_group,
      subset = !!rlang::enquo(observation_subset),
      label = ""
    )

  meta <- meta |>
    metalite::define_analysis(
      name = "ae_forestly",
      label = "Interactive forest plot"
    ) |>
    metalite::meta_build()

  meta
}
