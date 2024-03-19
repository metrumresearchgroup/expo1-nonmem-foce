#' ---
#' title: Data assembly (DA)
#' ---
#'
#' # Scope
#'
#' This document provides an example of the typical DA
#' structure that will be implemented on most projects.
#' This is not meant to be an exhaustive list and additional
#' derivations will likely be needed for all projects.
#' It also includes examples of how you can leverage the tools
#' within mrgda during your data assembly.


### Libraries ----------------------------
library(tidyverse)
library(here)
# MetrumRG developed
library(mrgda)
library(yspec)
library(lastdose)

### Directories ----------------------------
dataDir <- here("data", "source")
derived_path <- here("data", "derived", "examp-da.csv")

# Read in data specification ---------------
# Tell R where to find the yml
specLo <- here("data", "derived", "da-spec.yml")

# load in the spec file
spec <- ys_load(specLo)

# Read in source data ----------------------
# read_src_dir creates a named list of all the source data frames.
# This simplifies the process of calling in multiple domains and
# eliminates the need to provide specific file paths for each.
src_list <- read_src_dir(dataDir)


# Setup outputs -----------------------------------------------------------

# During data assemblies, we work with two types of variables: subject level
# and time-varying. 

# Subject level variables are those where there's only one unique
# value per subject. Sex, race and most demographic/baseline values are examples 
# of these. These variables typically add columns to the derived data set.

# Time-varying variables are those that update over time in the derived data set.
# Dosing records, PK observations and lab values are examples of these.

# To organize the variables into their respective types, we create output lists
# below. All subject level variables are saved in the derived$sl list, while 
# time-varying variables are in the derived$tv list.

# Prepare to save individual domain assembly
derived <- list()
# Subject level variables
derived$sl <- list()
# Time-varying variables
derived$tv <- list()

# Assemble demographics ---------------------------------------------------

# Demographics are located in the dm domain and we are interested in
# subjects who were not screen failures for the study.
dm0 <-
  src_list$dm %>%
  filter(ACTARM != "Screen Failure")

# Use the specification file to determine the numerical decode for SEX
print(spec$SEX)

# Assemble AGE and SEX from the dm domain
dm1 <-
  dm0 %>%
  transmute(
    USUBJID,
    AGE,
    SEX = if_else(SEX == "F", 1, 0), # Values assigned according to spec
    ACTARM,
    STUDY = STUDYID,
    SUBJ = SUBJID
  )
# Save the subject level demographic variables to the derived$sl list
derived$sl$dm <- dm1

# Assemble baseline vitals ------------------------------------------------
vs0 <-
  src_list$vs %>%
  # Filter to only baseline measurements
  filter(VSBLFL == "Y") %>% 
  # Filter vs domain to only subjects in the dm output
  filter(USUBJID %in% derived$sl$dm$USUBJID) 

# Filter to only baseline weight measurements
vs1 <-
  vs0 %>%
  filter(VSTESTCD %in% c("WEIGHT")) %>%
  # Transform to create WEIGHT column
  pivot_wider(names_from = "VSTESTCD", values_from = "VSSTRESN") %>% 
  transmute(
    USUBJID,
    WT = WEIGHT
  )
# Save the subject level variables to the derived$sl list
derived$sl$vs <- vs1

# Assemble baseline labs --------------------------------------------------
lb0 <-
  src_list$lb %>%
  # Filter to only baseline measurements
  filter(LBBLFL == "Y") %>% 
  # Filter vs domain to only subjects in the dm output
  filter(USUBJID %in% derived$sl$dm$USUBJID) 

# Filter to lab tests of interest
lb1 <-
  lb0 %>%
  filter(LBTESTCD %in% c("ALT", "AST", "BILI", "CREAT")) %>%
  select(USUBJID, LBTESTCD, LBSTRESN)

# Pivot to make each lab test it's own column
lb2 <- lb1 %>% pivot_wider(names_from = "LBTESTCD", values_from = "LBSTRESN") 

# Ensure one row per subject
assertthat::assert_that(
  nrow(lb2 %>% count(USUBJID) %>% filter(n > 1)) == 0
)

# Rename
lb3 <-
  lb2 %>% 
  select(
    USUBJID,
    ALT,
    AST,
    BILI,
    SCR = CREAT
  )

# Save the subject level variables to the derived$sl list
derived$sl$lb <- lb3

# Assemble dosing ---------------------------------------------------------
ex0 <-
  src_list$ex %>%
  # Filter to only treatment types of interest
  filter(EXTRT %in% c("PLACEBO", "XANOMELINE")) %>% 
  transmute(
    USUBJID,
    EVID = 1,
    DVID = 1,
    DOSE = EXDOSE,
    AMT = DOSE, # Note, on some projects AMT may need to be converted to new units
    DATETIME = lubridate::ymd(EXSTDTC)
  )

# Save the time varying dosing rows to the derived$tv list
derived$tv$ex <- ex0

# Assemble PK -------------------------------------------------------------
pc0 <-
  src_list$pc %>%
  filter(PCTEST == "XANOMELINE") %>%
  transmute(
    USUBJID,
    EVID = 0,
    DVID = 2,
    BLQ = if_else(grepl("<", PCORRES, fixed = TRUE), 1, 0),
    DV = if_else(BLQ == 0, PCSTRESN, NA_real_),
    LLOQ = PCLLOQ,
    DATETIME = lubridate::ymd_hms(PCDTC)
  )

# Save the time varying pk observation rows to the derived$tv list
derived$tv$pc <- pc0

# Combine domains ---------------------------------------------------------

# Check for duplicates in sl domain
assertthat::assert_that(
  purrr::map(derived$sl, ~ !duplicated(.x[["USUBJID"]])) %>% unlist() %>% any()
)

# Get all baseline variables
baseline_variables <- reduce(derived$sl, full_join)

nm0 <-
  # Bind all time varying data
  bind_rows(derived$tv) %>%
  # Left join on subject level data
  left_join(baseline_variables) %>%
  # Sort the data first by subject, 
  # then DATETIME so that all records are in the order they actually occurred. 
  arrange(
    USUBJID,
    DATETIME
  )

# Fill DOSE for all records by subject
nm1 <-
  nm0 %>% 
  group_by(USUBJID) %>%
  tidyr::fill("DOSE", .direction = "downup") %>%
  ungroup()

# Add TAFD and TAD columns using lastdose
nm2 <-
  nm1 %>%
  lastdose(include_tafd = TRUE, time_units = "hours") %>%
  mutate(
    TIME = TAFD
  )

# Create ID column
# If derived data exists, the IDs from there will be used so that
# the ID number stays consistent within subject across derived data versions
nm3 <-
  nm2 %>%
  assign_id(.subject_col = "USUBJID")

nm4 <-
  nm3 %>% 
  mutate(
    MDV = if_else(is.na(DV), 1, 0),
    C = ".",
    NUM = 1:n()
  )

derived$nm <- nm4 %>% select(names(spec))


# Write out data ----------------------------------------------------------

# Check data
# Use ys_check to confirm all columns match the specified type 
# and order defined in the spec file
ys_check(derived$nm, spec)

write_derived(
  .data = derived$nm,
  .spec = spec,
  .file = derived_path
)
