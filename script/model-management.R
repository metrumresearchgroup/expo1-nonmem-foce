# The model-management.R file is intended to be a scratchpad for doing things
# like defining, submitting, tagging, etc. your models. There is no need to keep
# a "record in code" of these activities because they can all be reconstructed
# later via functions like `run_log()`, as demonstrated in `model-summary.Rmd`
#
# The `Model Management Demo` (rendered from the `model-management-demo.Rmd`
# file) shows code for a range of these activities at different stages in the
# modeling process. It exists purely for reference; the intent is _not_ for you
# to replicate the full narrative.
# https://merge.metrumrg.com/expo1-nonmem-foce/posts/model-management-demo.html
#
# This script assumes you have already installed and set up bbi. For details
# on getting set up with bbr, see:
# https://metrumresearchgroup.github.io/bbr/articles/getting-started.html#setup


library(bbr)
library(tidyverse)

setwd(here::here("script"))

# define model dir and load tags
MODEL_DIR <- "../model/pk"
TAGS <- yaml::read_yaml("tags.yaml")





############################################
# CHECK FOR BBI INSTALLATION AND CONFIG 
############################################

# The code below checks that you have everything configured to begin modeling
# with bbr. This code can be deleted once everything is working correctly.

# To check if you have bbi installed and configured, run `bbi_version()`
bbi_version()
# If this doesn't return a version number, see 
# https://metrumresearchgroup.github.io/bbr/articles/getting-started.html#installing-bbi
# for installation details.


# The first time you are modeling in a new directory, you will need to "intialize" bbi.
# The bbi_init() function will create a bbi.yaml, with the default settings, in the 
# specified directory. 
# 
# To check you have initialized bbi, try to read in the `bbi.yaml` file.
file.path(MODEL_DIR, "bbi.yaml") %>% yaml::read_yaml() %>% names()
# If this errors, run `bbi_init()`:

bbi_init(.dir = MODEL_DIR,            # the directory to create the bbi.yaml in
         .nonmem_dir = "/opt/NONMEM", # location of NONMEM installation
         .nonmem_version = "nm75")  # default NONMEM version to use

# Note this only needs to be done _once for each folder_ you are modeling in. Once the bbi.yaml exists, 
# you will not need to run `bbi_init()` again unless you want to create another one; for example if you 
# move to modeling in a different directory.
#
# For more details on the `bbi.yaml` file and its usage, see:
# https://metrumresearchgroup.github.io/bbr/articles/getting-started.html#bbi-yaml-configuration-file


x <- formatC(seq(100, 106), width = 3, flag = "0")
runs <- paste0(here("model/pk/"), x)
args <- list(overwrite = TRUE)
mods <- lapply(runs, read_model)

submit_models(mods, .bbi_args=args)













