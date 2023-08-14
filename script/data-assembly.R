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


### Directories ----------------------------
dataDir = here("data", "source")
thisScript = "data-assembly.R"


# Read in source data ----------------------------
src_list <- mrgda::read_src_dir(here("data/source"))
