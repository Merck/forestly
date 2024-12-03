
library(arsenal)
library(stringr)



adae <- r2rtf::r2rtf_adae


load("~/forestly/data/forestly_adae_3grp.rda")


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

source("~/forestly/R/function_dataset0compare.R")

# Call the function to compare the datasets and save the summary
comparison_result <- compare_datasets(adae, forestly_adae_3grp, "compareresult_adae.html")

# Save the adae  dataset as an .rda file
save(adae, file = "~/forestly/data-raw/forestly_adae.rda")

