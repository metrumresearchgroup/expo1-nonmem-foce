### Libraries ----------------------------
library(tidyverse)
library(pmtables)
library(here)
library(bbr)
library(magrittr)
library(yaml)
library(glue)
library(pmparams)


### Directories ----------------------------
scriptDir <- here("script")
tabDir <- here("deliv", "table", "report")
if(!file.exists(tabDir)) dir.create(tabDir)

thisScript <- "pk-final-model-table-boot.R"

set.seed(5238974)

# Set table options ----------------------------
options(mrg.script = thisScript, 
        pmtables.dir = tabDir)


# get parameter names from yaml ------------------------------------------------
key <- here("script", "pk-parameter-key.yaml")

# Read in final model (non-bootstrap version) ----------------------------
run = "model/pk/106"
sum <- read_model(here(run)) %>% model_summary() 


# Extract PK parameters and generate values to be displayed in report table ----------------------------

##' For non-bootstrap run
param_df <- sum  %>% 
  define_param_table(.key = key) %>% 
  format_param_table()

param_df


# Read in bootstrap results for confidence intervals ----------------------------
boot <- readr::read_csv(here("data/boot/boot-106.csv"))

##' For bootstrap run
##' Instead of using the 95% CI, replace the value in 'estimate' 
##' with the median from the bootstrap runs and add the upper and lower values

boot_df <- boot %>%
  define_boot_table(.key = key,
                    .nonboot_estimates = sum) %>% 
  format_boot_table()

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
