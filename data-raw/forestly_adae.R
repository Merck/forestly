library(arsenal)
library(stringr)
library(dplyr)



adae <- r2rtf::r2rtf_adae



# Derive AREL from existing AEREL
adae <- adae %>%
  filter(TRTA != "Xanomeline High Dose") %>%
  mutate(AREL = case_when(
    AEREL %in% c("PROBABLE", "POSSIBLE") ~ "RELATED",
    AEREL %in% c("NONE", "REMOTE", "") ~ "NOT RELATED",
    TRUE ~ AEREL # Keep original AREL if none of the conditions are met
  ))


freq <- adae %>%
  count(AREL, AEREL) %>%
  arrange(desc(n))
print(freq)

freq2 <- forestly_adae %>%
  count(AREL, AEREL) %>%
  arrange(desc(n))
print(freq2)

str(adae$TRTA)
str(forestly_adae$TRTA)




adae$AEACN <- sample(
  x = c("DOSE NOT CHANGED", "DRUG INTERRUPTED", "DRUG WITHDRAWN", "NOT APPLICABLE", "UNKNOWN"),
  size = length(adae$USUBJID),
  prob = c(0.7, 0.1, 0.05, 0.1, 0.05), replace = TRUE
)

for (i in seq_along(adae$AEACN)) {
  adae$action_taken[i] <- switch(adae$AEACN[i],
    "DOSE NOT CHANGED" = "None",
    "DRUG INTERRUPTED" = "Interrupted",
    "DRUG WITHDRAWN" = "Discontinued",
    "NOT APPLICABLE" = "N/A",
    "UNKNOWN" = "Unknown",
    "''" = "None",
    tools::toTitleCase(tolower(adae$AEACN[i]))
  )
}

# CHECKS
# freq <- adae %>% count(AEACN) %>% arrange(desc(n))
# print(freq)
#
#
# adae %>%
#   select(USUBJID, TRTAN, TRTA, TRTSDT, ASTDT, ADURN, AETERM, AEREL, AESEQ, TRTEMFL, AREL, AEACN) %>%
#   slice(1) %>%
#   print()
# forestly_adae %>%
#   select(USUBJID, TRTAN, TRTA, TRTSDT, ASTDT, ADURN, AETERM, AEREL, AESEQ, TRTEMFL, AREL, AEACN) %>%
#   slice(1) %>%
#   print()
