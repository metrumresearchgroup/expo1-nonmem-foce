#' ---
#' title: Collect bootstrap parameter estimates
#' ---
library(tidyverse)
library(bbr)
library(here)
library(glue)

boot_dir <- here("model", "pk", "boot")
mod_id <- "106" # model name of original model

####################################
# pull in raw boostrap param estimates
# take a quick look at them
# save them out to csv
####################################

boot <- param_estimates_batch(boot_dir)

# Check for runs with [bbr/bbi] error messages
# Errors mean [bbi] failed to parse model outputs
# If there are any, you should check those models 
#   manually to see what happened
err <- filter(boot, !is.na(error_msg))
nrow(err)

# Do a quick comparison of the original estimates and the new estimates
# you can do `summary(boot)`, but the code below makes a nice tibble to look at
orig_mod <- read_model(here("model", "pk", mod_id))
param_estimates_compare(
  boot,
  orig_mod
)

# Write bootstrap parameter estimates to file
boot %>%
  select(run, starts_with(c("THETA", "OMEGA", "SIGMA"))) %>%
  write_csv(here("data", "boot", glue("boot-{mod_id}.csv")))

####################################
# check NONMEM heuristics and termination codes for all models
# and save both to csv
####################################

# Get termination codes and save
term <- select(boot, run, termination_code)

count(term, termination_code)

write_csv(term, here("data", "boot", glue("boot-{mod_id}-term.csv")))

# Get heuristics by running full summary for each model
# (this will take 1-2 minutes for 1000 models)
smry <- summary_log(boot_dir)
heu <- smry %>% 
  filter(any_heuristics) %>% 
  select(run, where(is.logical), -needed_fail_flags) 

# Check how many of each heuristic was found
heu %>%
  select(-run, -any_heuristics) %>%
  summarise_all(sum, na.rm = TRUE) %>% 
  pivot_longer(everything())

# Save any heuristics columns to a csv.
# This could be useful for including in a parameter table footer, 
# if you end up filtering out any runs 
# or need to summarize runs that didn't minimize, etc.
write_csv(heu, here("data", "boot", glue("boot-{mod_id}-heuristics.csv")))
