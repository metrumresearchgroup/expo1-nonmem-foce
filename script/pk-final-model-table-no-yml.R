### Libraries ----------------------------
library(tidyverse)
library(pmtables)
library(here)
library(bbr)
library(magrittr)

### Directories ----------------------------
scriptDir <- here("script")
tabDir <- here("deliv", "table", "report")
if(!file.exists(tabDir)) dir.create(tabDir)

thisScript <- "pk-final-model-table-no-yml.R"

set.seed(5238974)


# Helper functions ----------------------------
source(here("script", "functions-table.R"))


# Set table options ----------------------------
options(mrg.script = thisScript, 
        pmtables.dir = tabDir)


# Read in final model ----------------------------
run = "model/pk/106"
sum <- read_model(here(run)) %>% model_summary() 


# get parameter names  ---------------------------- ----------------------------
##' Note the preferred method is to move this to an external yaml file
##' name = THETA, OMEGA, SIGMA
##' 
##' abb = abbreviation you want to appear in the parameter table (use latex coding)
##' 
##' desc = description you want to appear in the parameter table
##' 
##' panel = indicate which panel each parameter should appear under. 
##' Current options include:
##'   panel=="struct" ~ "Structural model parameters"
##'   panel=="cov" ~ "Covariate effect parameters"
##'   panel=="IIV" ~ "Interindividual covariance parameters" (off-diagonals << function takes care of this)
##'   panel=="IIV" ~ "Interindividual variance parameters" (diagonals << function takes care of this)
##'   panel=="IOV"  ~ "Interoccasion variance parameters"
##'   panel=="RV" ~ "Residual variance"
##'   Additional options can be added to `getPanelName` function in `functions-table.R`
##' 
##' trans = define how do you want the parameter to be transformed
##'   there are a finite options currently coded up (see below) but you're welcome
##'   add new options. Current options include:
##'        "none"       - untransformed parameters, e.g. THETAs or off-diagonals
##'        "logTrans"   - THETAs estimated in the log domain
##'        "logitTrans" - THETAs estimated using a logit transform
##'        "lognormalOm"- for log-normal omegas e.g. CL = THETA(1) * exp(ETA(1)) - returns est.+CV%
##'        "OmSD"       - for omegas where you want to return SD only - returns estimate & SD
##'                     - use this for additive omegas e.g. CL = THETA(1) + ETA(1) 
##'        "logitOmSD"  - for omegas using logit transform - returns estimate & SD (calculated with logitnorm package)
##'                     - this option requires you provide the associated THETA separated with a "~"
##'                     - e.g. "logitOmSD ~ THETA3"
##'        "addErr"     - for additive error terms (coded using SIGMA in $ERROR) - returns est.+ SD
##'        "propErr"    - for proportional error terms (coded using SIGMA in $ERROR) - returns est.+CV%

paramKey = tribble(
  ~name, ~abb, ~desc, ~panel, ~trans,
  "THETA1",  "KA (1/h)", "First order absorption rate constant",   "struct", "logTrans",
  "THETA2", "V2/F (L)",  "Apparent central volume",                "struct", "logTrans",
  "THETA3", "CL/F (L/h)", "Apparent clearance",                    "struct", "logTrans",
  "THETA4", "V3/F (L)",  "Apparent peripheral volume",             "struct", "logTrans",
  "THETA5", "Q/F (L/h)", "Apparent intercompartmental clearance",  "struct", "logTrans",
  "THETA6", "$\\text{CL/F}_{eGFR}$", "eGFR effect on CL/F",        "cov",    "none",
  "THETA7", "$\\text{CL/F}_{AGE}$", "Age effect on CL/F",          "cov",    "none",
  "THETA8", "$\\text{CL/F}_{ALB}$", "Serum albumin effect on CL/F","cov",    "none",
  
  "OMEGA11", "IIV-KA",   "Variance of absorption",     "IIV", "lognormalOm",
  "OMEGA22", "IIV-V2/F", "Variance of central volume", "IIV", "lognormalOm",
  "OMEGA33", "IIV-CL/F", "Variance of clearance",      "IIV", "lognormalOm",
  
  "OMEGA21", "V2/F-KA", "Covariance of V2/F - KA",    "IIV", "none",
  "OMEGA31", "CL/F-KA", "Covariance of CL/F - KA",    "IIV", "none",
  "OMEGA32", "CL/F-V2/F", "Covariance of CL/F - V2/F","IIV", "none",

  "SIGMA11", "Proportional", "Variance", "RV", "propErr"
)


# Extract PK parameters and generate values to be displayed for report table ----------------------------
param_df <- sum  %>% 
  param_estimates() %>% 
  mutate(name = gsub("[[:punct:]]", "", parameter_names)) %>% 
  inner_join(paramKey, by = "name")  %>%    # add names and labels to be used in the table
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


# Define footnotes ---------------------------- ----------------------------
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
  stem = "preview-no-yml-param-table",
  show_pdf = interactive()
)
