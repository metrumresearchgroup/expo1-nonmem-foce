#' ---
#' title: "Generate bootstrap data sets and estimation control streams"
#' output: github_document
#' editor_options: 
#'   chunk_output_type: console
#' ---

#' # Introduction
#' 
#' This script demonstrates how to run a bootstrap. It will sample the data to
#' create bootstrapped data sets, create the model files that point to this 
#' sampled data, and then submit the models for execution.


# Required packages
library(data.table)
library(bbr)
library(tidyverse)
library(glue)
library(mrgmisc)
library(whisker)

#' # Preprocessing

#' Add `boot_dir` and create appropriate directories.  
pk_model_dir <- "../model/pk"
boot_dir <- file.path(pk_model_dir,"boot")
boot_data_dir <- file.path(boot_dir, "data")

if(!dir.exists(boot_dir)) dir.create(boot_dir)
if(!dir.exists(boot_data_dir)) dir.create(boot_data_dir)

#' Create template from original `.ctl` file, removing `COV`, `tables`, `MSF`

template_ctl <- readLines(file.path(pk_model_dir, "boot-106-template.ctl"))

#' In the template:
#' - $DATA ../data/{{run_num}}.csv
#' - Optionally add OID at the end of $INPUT; but not required

nmdata <- read_csv("../data/derived/analysis3.csv", na = '.')
nmdata <- filter(nmdata, is.na(C))

#' ## Looped over multiple times

#' - Needs examples for stratifcation with continuous or categorical data

RUN <- seq(1000)

make_boot_run <- function(n, data, template, boot_dir, strat_cols = NULL, 
                          overwrite = FALSE,  seed = 21181) {
  
  if (n %% 100 == 0) message(glue("Created {n} bootstrap data sets"))
  mod_name <- pad_left(n, 3)
  
  mod_path <- glue("{boot_dir}/{mod_name}")
  
  if(file.exists(paste0(mod_path, ".yaml")) && !overwrite) {
    return(read_model(mod_path))
  }

  data_new <- resample_df(
    data,
    key_cols = "ID",
    strat_cols = strat_cols
  )
  
  data_new <- rename(data_new, OID = ID, ID = KEY)
  
  data_new <- select(data_new, unique(c("C", names(data), "OID")))
  
  csv_file <- glue("{boot_dir}/data/{mod_name}.csv")
  
  fwrite(data_new, csv_file , na = '.', quote = FALSE)
  
  new_ctl <- whisker.render(template, list(run_num = mod_name))
  
  write_file(new_ctl, file = paste0(mod_path, ".ctl"))
  
  mod <- new_model(
    mod_path,
    .description = glue("bootstrap {mod_name}"), 
    .overwrite = overwrite
  )
  
  mod
}


#' For each proposed bootstrap run    
#' - Create a bootstrap version of the data set
#'   - Stratify on `RF` and `CP`
#'   - Write out to csv file
#' - Create a bootstrap version of the ctl file
#'   - Update with bootstrap data set
#' - Create a model object

set.seed(12345)

models <- map(
  RUN, 
  data = nmdata, 
  .f = make_boot_run, 
  template = template_ctl, 
  boot_dir = boot_dir, 
  strat_cols = c("RF", "CP"),
  overwrite = TRUE
)

#' # Submit models to run

#' This is where you actually run the models. If the output directories already
#' exist then these have already been run and you don't need to run them again.

#' Submit first model locally to check that it runs fine.  This code is for 
#' example only and is commented out.

# models[[1]] %>% submit_model(.mode = "local", .bbi_args=list(overwrite=TRUE))

out <- submit_models(
  models,
  .config_path = "../model/pk/bbi.yaml",
  .bbi_args = list(overwrite = TRUE, threads = 1)
)


#' # Conclusion
#' 
#' Once the models are finished running, see `boot-collect.R` which 
#' demonstrates how to extract the parameter estimates from these runs.
#' 
#' # Session info

Sys.getenv("AMI_NAME")

sessionInfo()
