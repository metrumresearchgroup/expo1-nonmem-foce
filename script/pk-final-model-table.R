### Libraries ----------------------------
library(tidyverse)
library(pmtables)
library(here)
library(bbr)
library(magrittr)
library(yaml)


### Directories ----------------------------
scriptDir <- here("script")
tabDir <- here("deliv", "table", "report")
if(!file.exists(tabDir)) dir.create(tabDir)

thisScript <- "pk-final-model-table.R"

set.seed(5238974)


# Helper functions ----------------------------
source(here("script", "functions-table.R"))


# Set table options ----------------------------
options(mrg.script = thisScript, 
        pmtables.dir = tabDir)


# Read in final model ----------------------------
run = "model/pk/106"
sum <- read_model(here(run)) %>% model_summary() 


# get parameter names from yaml ------------------------------------------------
key <- yaml_as_df(here("script", "pk-parameter-key.yaml"))


# Extract PK parameters and generate values to be displayed for report table ----------------------------
param_df <- sum  %>% 
  param_estimates() %>% 
  mutate(name = gsub("[[:punct:]]", "", parameter_names)) %>% 
  inner_join(key, by = "name")  %>%    # add names and labels to be used in the table
  checkTransforms() %>%       # check for associated THETAs (e.g. for logit transformations)
  defineRows() %>%            # define series of T/F variables
  getValueSE() %>%            # define which value and se are required
  get95CI() %>%               # get upper/lower ci - determined using the SE and estimate
  formatValues() %>%          # back transform as needed, round using 'sig' and combine columns where needed
  formatGreekNames() %>%      # format the labels to display greek symbols
  getPanelName() %>%          # Define panel names based on parameter type
  dplyr::select(type, abb, greek, desc, value, ci, shrinkage) %>%  # select columns of interest
  as.data.frame

param_df 


# Define footnotes --------------------------------------------------------
footerSource = paste0('source: ', thisScript)
footAbbrev = "Abbreviations: CI = confidence intervals; 
                        SE = standard error"
footAbbrev_Om = "Abbreviations: CI = confidence intervals; 
                        Corr = Correlation coefficient;
                        CV = coefficient of variation;
                        SD = standard deviation;
                        SE = standard error" 
footDerive1 = "Confidence intervals = estimate $\\pm$ 1.96 $\\cdot$ SE"  #\u00b1
footDerive2 = "CV\\% of log-normal omegas = sqrt(exp(estimate) - 1) $\\cdot$ 100"
footDerive3 = "CV\\% of sigma = sqrt(estimate) $\\cdot$ 100"
footLog = "Parameters estimated in the log-domain were back-transformed for clarity"




##  FIXED EFFECTS table ----------------------------
fixed = param_df %>% 
  filter(str_detect(type, "Struct") | 
           str_detect(type, "effect")) %>%
  select(-shrinkage) %>% 
  st_new() %>% 
  st_panel("type") %>% 
  st_center(desc = col_ragged(5.5),
            abb = "l") %>% 
  st_blank("abb", "greek", "desc") %>% 
  st_rename("Estimate" = "value", 
            "95\\% CI" = "ci") %>% 
  st_files(output = "pk-param-final-fixed.tex") %>% 
  st_notes(footLog, footAbbrev, footDerive1) %>% 
  st_noteconf(type = "minipage", width = 1) %>% 
  stable() %T>% 
  # st2report(ntex = 2) %T>%
  stable_save()


##  RANDOM EFFECTS table ----------------------------
random = param_df %>% 
  filter(str_detect(greek, "Omega") | 
           str_detect(type, "Resid")) %>%
  select(-desc) %>%
  st_new %>% 
  st_panel("type") %>% 
  st_center(abb = "l") %>% 
  st_blank("abb", "greek") %>% 
  st_rename("Estimate" = "value", 
            "95\\% CI" = "ci",
            "Shrinkage (\\%)" = "shrinkage") %>% 
  st_files(output = "pk-param-final-random.tex") %>% 
  st_notes(footAbbrev_Om, footDerive2, footDerive3) %>% 
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
  stem = "preview-final-param-table",
  show_pdf = interactive()
)
