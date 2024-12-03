
library(arsenal)
library(stringr)


adsl <- r2rtf::r2rtf_adsl




load("~/forestly/data/forestly_adsl_3grp.rda")



source("~/forestly/R/function_dataset0compare.R")


#Compare ADSL dataset

freq <- forestly_adsl_3grp %>% count(TRT01A, TRTA) %>%
  arrange(desc(n))
print(freq)

# Derive TRTA from existing AEREL
adsl <- adsl %>%
  mutate(
   TRTA = str_replace_all(as.character(TRT01A), "Xanomeline ", "") %>% as.factor()  # Remove "Xanomeline " and convert to factor
  )

# Define the desired order of levels
desired_levels <- c("Placebo", "Low Dose", "High Dose")

# Standardize TRTA in adae
adsl$TRTA <- factor(adsl$TRTA, levels = desired_levels)


# Save the adae  dataset as an .rda file
save(adsl, file = "~/forestly/data-raw/forestly_adsl.rda")
