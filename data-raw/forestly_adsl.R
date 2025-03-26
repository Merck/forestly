library(dplyr)

adsl <- r2rtf::r2rtf_adsl

# Derive TRTA
forestly_adsl_3grp <- adsl |>
  mutate(
   TRTA = TRT01A |> as.factor()
  )

# Define the desired order of levels
desired_levels <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")

# Standardize TRTA in adae
forestly_adsl_3grp$TRTA <- factor(forestly_adsl_3grp$TRTA, levels = desired_levels)
usethis::use_data(forestly_adsl_3grp, overwrite = TRUE)
# Keep two treatment group
forestly_adsl <- forestly_adsl_3grp[forestly_adsl_3grp$TRTA %in% c("Placebo", "Xanomeline Low Dose"),]
usethis::use_data(forestly_adsl, overwrite = TRUE)
