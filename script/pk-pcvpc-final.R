#' --- 
#' title: pcVPC with the vpc package
#' ---
#' 
#' # Scope
#' 
#' This document illustrates the mechanics of using the vpc package to 
#' create a prediction-corrected vpc. 
#' 
#' We will create one vpc plot: pred-corrected vpc stratified by STUDY.
#' 

#' # Required packages
library(tidyverse)
library(glue)
library(yspec)
library(mrgsolve)
library(mrggsave)
library(vpc)
library(bbr)
library(here)
source(here("script/functions-model.R"))

options(mrggsave.dir = here("deliv/figure"), mrg.script = "pk-pcvpc-final.R")
options(mrgsolve.project = here("script/model"))

mrg_vpc_theme = new_vpc_theme(list(
  sim_pi_fill = "steelblue3", sim_pi_alpha = 0.5,
  sim_median_fill = "grey60", sim_median_alpha = 0.5
))

spec <- ys_load(here("data/spec/analysis3.yml"))
lab <- ys_get_short_unit(spec, parens = TRUE, title_case = TRUE)

#' 
#' # Model
#' 
runno <- 106

#' This should reflect `../model/pk/106.ctl`
mod <- mread(glue("{runno}.mod"))

#' 
#' # Data
#' This returns the complete data set by passing `.superset = TRUE`
#' 
data <- nm_join(glue(here("model/pk/{runno}")), .superset = TRUE)
data <- filter(data, is.na(C))

#' Simulate `PRED` for the records that are BLQ, otherwise we'll use the 
#' value coming from NONMEM
anyNA(data$PRED) # TRUE
out <- mrgsim(zero_re(mod), data, digits = 5)
data <- mutate(data, PRED = ifelse(BLQ > 0, out$Y, PRED))
anyNA(data$PRED) # FALSE


#' # Set up the simulation
#' 
#' Create a function to simulate out one replicate
sim <- function(rep, data, model) {
  mrgsim(
    model, 
    data = data,
    carry_out = "EVID,STUDYN,LDOS,DOSE,PRED",
    recover = "STUDY,C,USUBJID,ACTARM,RF,Renal,Hepatic", 
    Req = "Y", 
    output = "df", 
    quiet  = TRUE
  ) %>%  mutate(irep = rep)
}

#' Simluate data
isim <- seq(200)

set.seed(86486)
sims <- lapply(
  isim, sim, 
  data = data, 
  mod = mod
) %>% bind_rows()

#' Filter both the observed and simulated data
#' For the observed data, we only want actual observations that weren't BLQ
#' For the simulated data, we take simulated observations that were above LQ
fdata <- filter(data, EVID==0, BLQ == 0)
fsims <- filter(sims, EVID==0, Y >= 10)

#' # Create the plot
#' 
#' Pass observed and simulated data into vpc function
p1 <- vpc(
  obs = fdata,
  sim = fsims,
  pred_corr = TRUE,
  stratify = "STUDY",
  sim_cols=list(dv="Y", sim="irep"), 
  log_y = TRUE,
  pi = c(0.05, 0.95),
  ci = c(0.025, 0.975), 
  vpc_theme = mrg_vpc_theme
) 

p1 <- 
  p1 +  
  theme_bw() + 
  xlab(lab$TIME) + 
  ylab("Prediction-corrected concentration (ng/mL)")

p1

mrggsave(p1, stem = "pk-vpc-{runno}-pred-corr", height = 6)
