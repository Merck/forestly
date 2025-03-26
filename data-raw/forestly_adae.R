adae <- r2rtf::r2rtf_adae

# factor TRTA

# Create variable AEACN by randomly assigned from c("DRUG INTERRUPTED", "DOSE REDUCED", "DRUG WITHDRAWN",
# "NOT APPLICABLE", "DOSE NOT CHANGED")

# adae$AEACN <- ...

# Create variable ATOXGRN by randomly assigned to c(1,2,3,4,5)

# adae$ATOXGRN <- ...

# Create AREL by Renaming AEREL, remove AEREL

# Assign adae to forestly_adae_3grp
forestly_adae_3grp <- adae

# keep only two treatment arm, similar to data-raw/forestly_adsl.R



