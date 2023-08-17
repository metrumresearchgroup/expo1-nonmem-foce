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
#' It includes examples of how you can leverage the tools
#' within mrgda during your data assembly.


### Libraries ----------------------------
library(tidyverse)
library(mrgda)
library(here)
library(yspec)

### Directories ----------------------------
dataDir = here("data", "source")
thisScript = "data-assembly.R"

# Read in data specification ---------------
# Tell R where to find the yml
specLo = here("data", "spec", "analysis3.yml")

# load in the spec file
spec = ys_load(specLo)

# Read in source data ----------------------
src_list <- mrgda::read_src_dir(here("data/source"))


# Setup outputs -----------------------------------------------------------

# The purpose is to save similar data types together, meaning
# all longitudinal variables (subject level) are saved into
# the derived$sl list and time-varying variables in the
# derived$tv list.

# Prepare to save individual domain assembly
derived <- list()
# Subject level
derived$sl <- list()
# Time varying
derived$tv <- list()

# Assemble demographics ---------------------------------------------------
dm0 <-
  src_list$dm %>% 
  filter(ACTARM != "Screen Failure")

# Use the specification file to determine the numerical decode for SEX
spec$SEX

# Assemble AGE and SEX from the dm domain
dm1 <-
  dm0 %>% 
  transmute(
    USUBJID,
    AGE,
    SEX = if_else(SEX == "F", 1, 0),
    RACE = case_when(
      RACE == "WHITE" ~ 1,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 2,
      TRUE ~ 3
    ),
    ACTARM,
    STUDY = STUDYID,
    SUBJID
  )
# Save the subject level variables to the derived$sl list
derived$sl$dm <- dm1

# Assemble baseline vitals ------------------------------------------------
vs0 <-
  src_list$vs %>% 
  filter(VSBLFL == "Y") %>% # Filter to only baseline measurements
  filter(USUBJID %in% derived$sl$dm$USUBJID) # Filter vs domain to only subjects in the dm output

# Filter to only baseline weight measurements 
vs1 <-
  vs0 %>% 
  filter(VSTESTCD %in% c("WEIGHT")) %>% 
  pivot_wider(names_from = "VSTESTCD", values_from = "VSSTRESN") %>% # Transform to create WEIGHT column
  transmute(
    USUBJID,
    WT = WEIGHT
  )
# Save the subject level variables to the derived$sl list
derived$sl$vs <- vs1

# Assemble baseline labs --------------------------------------------------
lb0 <-
  src_list$lb %>% 
  filter(LBBLFL == "Y") %>% # Filter to only baseline measurements
  filter(USUBJID %in% derived$sl$dm$USUBJID) # Filter vs domain to only subjects in the dm output

# Filter to only baseline weight measurements 
lb1 <-
  lb0 %>% 
  filter(LBTESTCD %in% c("ALT", "AST", "BILI", "CREAT")) %>% 
  pivot_wider(names_from = "LBTESTCD", values_from = "LBSTRESN") %>% # Transform to create one column per lab test
  # Each lab test provided on individual row in the lb domain. The code below is used
  # to have one row per subject
  group_by(USUBJID) %>% 
  tidyr::fill("ALT", "AST", "BILI", "CREAT", .direction = "downup") %>% 
  slice(1) %>% 
  ungroup() %>% 
  transmute(
    USUBJID,
    ALT,
    AST,
    BILI,
    SCR = CREAT
  )
# Save the subject level variables to the derived$sl list
derived$sl$lb <- lb1

# Assemble dosing ---------------------------------------------------------
ex0 <-
  src_list$ex %>% 
  transmute(
    USUBJID,
    EXTRT,
    DOSE = EXDOSE,
    AMT = DOSE,
    VISIT,
    DATETIME = lubridate::ymd(EXSTDTC),
    ENDDTC = lubridate::ymd(EXENDTC)
  )

