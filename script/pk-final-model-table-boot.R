### Libraries ----------------------------
library(tidyverse)
library(pmtables)
library(here)
library(bbr)
library(magrittr)
library(yaml)
library(glue)


### Directories ----------------------------
scriptDir <- here("script")
tabDir <- here("deliv", "table", "report")
if(!file.exists(tabDir)) dir.create(tabDir)

thisScript <- "pk-final-model-table-boot.R"

set.seed(5238974)


# Helper functions ----------------------------
source(here("script", "functions-table.R"))


# Set table options ----------------------------
options(mrg.script = thisScript, 
        pmtables.dir = tabDir)


# get parameter names from yaml ------------------------------------------------
key <- yaml_as_df(here("script", "pk-parameter-key.yaml"))


# Read in bootstrap results for confidence intervals ----------------------------
boot <- readr::read_csv(here("data/boot/boot-106.csv"))

bootParam = boot %>%
  param_estimates_compare() %>%
  rename(estimate = `50%`, lower = `2.5%`, upper = `97.5%`) %>%
  mutate(name = gsub("[[:punct:]]", "", parameter_names)) %>% 
  inner_join(key, by = "name")


# Read in final model (non-bootstrap version) ----------------------------
run = "model/pk/106"
sum <- read_model(here(run)) %>% model_summary() 


# Extract PK parameters and generate values to be displayed for report table ----------------------------

##' For non-bootstrap run
param_df <- sum  %>% 
  param_estimates() %>% 
  mutate(name = gsub("[[:punct:]]", "", parameter_names)) %>% 
  inner_join(key, by = "name")  %>%    # add names and labels to be used in the table
  checkTransforms() %>%       # check for associated THETAs (e.g. for logit transformations)
  defineRows() %>%        # define series of T/F variables
  getValueSE() %>%            # define which value and se are required
  get95CI() %>%               # get upper/lower ci - determined using the SE and estimate
  formatValues() %>%      # back transform as needed, round using'sig' and combine columns where needed
  formatGreekNames() %>%  # format the labels to display greek symbols
  getPanelName() %>%          # Define panel names based on parameter type
  dplyr::select(type, abb, greek, desc, value, ci, shrinkage) %>%  # select columns of interest
  as.data.frame

param_df


##' For bootstrap run
##' Instead of using 'get95CI()' to get the 95% CI, replace the value in 'estimate' 
##' with the median from the bootstrap runs and add the upper and lower values

boot_df <- bootParam %>%
  left_join(sum %>% 
              param_estimates() %>% select(parameter_names, fixed), 
            by = "parameter_names") %>% 
  mutate(value = estimate) %>% 
  checkTransforms() %>%       # check for associated THETAs (e.g. for logit transformations)
  defineRows() %>% 
  backTrans_log() %>%         # back-transform any log thetas 
  backTrans_logit() %>%       # back-transform any logit thetas
  formatValues_boot() %>% 
  dplyr::select(abb, desc, boot_value, boot_ci) 

boot_df

# join tables ----------------------------
bootParam = left_join(param_df, boot_df, by = c("abb", "desc"))


# Define footnotes ---------------------------- ----------------------------
footAbbrev_fixed = "Abbreviations: CI = confidence interval"
footAbbrev_random = "Abbreviations: CI = confidence interval; 
                        Corr = correlation coefficient;
                        CV = coefficient of variation"
footDeriveBoot = glue("The confidence interval was determined from the 
                      2.5th and 97.5th percentiles of the non-parametric 
                      bootstrap (n={nrow(boot)}) estimates.")
footDerive1 = "Confidence interval = estimate $\\pm$ 1.96 $\\cdot$ SE"  #\u00b1
footDerive2 = "CV\\% of log-normal omegas = sqrt(exp(estimate) - 1) $\\cdot$ 100"
footDerive3 = "CV\\% of sigma = sqrt(estimate) $\\cdot$ 100"
footLog = "Parameters estimated in the log-domain were back transformed for clarity"
footBoot = glue("The non-parametric bootstrap analysis included {nrow(boot)} replicates")



##  FIXED EFFECTS table ----------------------------
fixed = bootParam %>% 
  filter(str_detect(type, "Struct") | 
           str_detect(type, "effect")) %>%
  select(-shrinkage, -ci) %>% 
  st_new() %>% 
  st_panel("type") %>% 
  st_span("Final model", value) %>% 
  st_span("Non-parametric bootstrap", boot_value:boot_ci) %>% 
  st_center(desc = col_ragged(5.5),
            abb = "l") %>% 
  st_blank("abb", "greek", "desc") %>% 
  st_rename("Estimate" = "value",
            "Median" = "boot_value",
            "95\\% CI" = "boot_ci") %>% 
  st_files(output = "pk-param-boot-fixed.tex") %>% 
  st_notes(footLog, footDeriveBoot, footAbbrev_fixed) %>% 
  st_noteconf(type = "minipage", width = 1) %>% 
  stable() %T>% 
  # st2report(ntex = 2) %T>%
  stable_save()



##  RANDOM EFFECTS table ----------------------------
random = bootParam %>% 
  filter(stringr::str_detect(greek, "Omega") | 
           str_detect(type, "Resid")) %>%
  select(-desc, -ci)  %>%
  st_new %>% 
  st_panel("type") %>% 
  st_span("Final model", value:shrinkage) %>% 
  st_span("Non-parametric bootstrap", boot_value:boot_ci) %>% 
  st_center(abb = "l") %>% 
  st_blank("abb", "greek") %>% 
  st_rename("Estimate" = "value", 
            "Median" = "boot_value", 
            "95\\% CI" = "boot_ci",
            "Shrinkage (\\%)" = "shrinkage") %>% 
  st_files(output = "pk-param-boot-random.tex") %>% 
  st_notes(footDeriveBoot, footAbbrev_random,
           footDerive2, footDerive3) %>% 
  st_noteconf(type = "minipage", width = 1) %>% 
  stable() %T>% 
  # st2report(ntex = 2) %T>%
  stable_save()


##  Save tables out to pdf preview ----------------------------
# Check they fit within the report margins
st2report(
  list(fixed, random),
  ntex = 2,
  output_dir = tabDir,
  stem = "preview-bootstrap-param-table",
  show_pdf = interactive()
)
