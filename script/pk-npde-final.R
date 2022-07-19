#' --- 
#' title: NPDE diagnostics
#' ---

#' # Required packages
library(tidyverse)
library(bbr)
library(yspec)
library(mrggsave)
library(pmplots)
library(here)

options(mrggsave.dir = here("deliv", "figure"), mrg.script = "pk-npde-final.R")
options(bbr.verbose = FALSE)

spec <- ys_load(here("data", "spec", "analysis3.yml"))
tab <- nm_join(here("model", "pk", "106"))

data <- 
  tab %>% 
  filter(EVID==0) %>% 
  ys_add_factors(spec)
  
p <- npde_time(data); p

mrggsave_last(tag = "vs-time")

p + facet_wrap(~RF_f)

mrggsave_last(tag = "vs-time-by-rf")

p <- npde_tad(data) + facet_wrap(~RF_f)
p
mrggsave_last(tag = "vs-tad-by-rf")

npde_hist(data, bins = 15) + facet_grid(CP_f~RF_f)
mrggsave_last(tag = "hist")

npde_pred(data, xname = "Mockdralazine concentration (ng/ml)") + 
  facet_wrap(~RF_f)

mrggsave_last(tag = "vs-pred-by-rf")
