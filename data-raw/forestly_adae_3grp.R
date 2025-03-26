
library(arsenal)
library(stringr)
library(dplyr)



adae <- r2rtf::r2rtf_adae



# Derive AREL from existing AEREL
adae <- adae %>%
  mutate(AREL = case_when(
    AEREL %in% c("PROBABLE", "POSSIBLE") ~ "RELATED",
    AEREL %in% c("NONE", "REMOTE", "") ~ "NOT RELATED",
    TRUE ~ AEREL  # Keep original AREL if none of the conditions are met
  ),
  TRTA = str_replace_all(as.character(TRTA), "Xanomeline ", "") %>% as.factor()  # Remove "Xanomeline " and convert to factor
  )

freq <- adae %>% count(AREL, AEREL) %>%
  arrange(desc(n))
print(freq)

str(adae$TRTA)
str(forestly_adae_3grp$TRTA)

# Define the desired order of levels
desired_levels <- c("Placebo", "Low Dose", "High Dose")

# Standardize TRTA in adae
adae$TRTA <- factor(adae$TRTA, levels = desired_levels)



