#' --- 
#' title: VPC with the vpc package
#' ---
#' 
#' # Scope
#' 
#' This document illustrates the mechanics of using the vpc package to 
#' get people started. Pred-corrected VPC is only considered for EMA filings, 
#' where the agency prefers pred-correction.
#' 

#' # Required packages
library(tidyverse)
library(yspec)
library(mrgsolve)
library(mrggsave)
library(vpc)
library(glue)
library(bbr)
library(here)

options(mrggsave.dir = here("deliv/figure"), mrg.script = "pk-vpc-final.R")
options(mrgsolve.project = here("script/model"))

mrg_vpc_theme = new_vpc_theme(list(
  sim_pi_fill = "steelblue3", sim_pi_alpha = 0.5,
  sim_median_fill = "grey60", sim_median_alpha = 0.5
))

#' # Data
#' 
#' First, we have some observations that are BQL that need to be
#' dealt with.  We use a complete data approach, where when we 
#' simulate, we simulate the complete design or all intended samples 
#' and then apply any BLQ rules accordingly (e.g., drop values less than 
#' BLQ).
#' 

#' 
#' The (full) analysis data set. Note we are not dropping out any values 
#' that were ignored in that analysis (i.e., BLQ values) because we want 
#' to simulate the same number of observations that were taken in the 
#' original study.
#' 
runno <- 106

#' 
#' Using the `.superset = TRUE` argument here to get the complete data; don't 
#' forget to filter out other records that may have been ignored in the run. 
#' 
data <- nm_join(here("model/pk/106"), .superset = TRUE)
data <- filter(data, is.na(C))

spec <- ys_load(here("data/spec/analysis3.yml"))
spec <- ys_namespace(spec, "long")
lab <- ys_get_short_unit(spec, parens = TRUE, title_case = TRUE)
data <- ys_add_factors(data, spec)
data <- mutate(data, Hepatic = CP_f, Renal = RF_f)

#' # Simulate the vpc
#' 
#' ## Load the mrgsolve model
#' 
#' This should reflect `../model/pk/106.ctl`
mod <- mread(glue("{runno}.mod"))

#' ## Take single dose only
sad_mad <- filter(data, STUDYN <= 2) %>% select(-C,-USUBJID)

#' # Set up the simulation
#' 
#' Create a function to simulate out one replicate
sim <- function(rep, data, model) {
  mrgsim(
    model, 
    data = data,
    carry_out = "EVID,STUDYN,LDOS,DOSE",
    recover = "STUDY,C,USUBJID,ACTARM,RF,Renal,Hepatic", 
    Req = "Y", 
    output = "df", 
    quiet  = TRUE
  ) %>%  mutate(irep = rep)
}

#' Simluate data

#' 200 replicates
isim <- seq(200)

set.seed(86486)
sims <- lapply(
  isim, sim, 
  data = sad_mad, 
  mod = mod
) %>% bind_rows()

sum(sims$Y)

#' Filter both the observed and simulated data
#' For the observed data, we only want actual observations that weren't BLQ
#' For the simulated data, we take simulated observations that were above LQ
fsad_mad <-  filter(sad_mad,  EVID==0, BLQ ==0)
fsims <- filter(sims, EVID==0, Y >= 10)

#' This will dose-normalize the concentrations so we can put them all on 
#' the same plot
fsad_mad <-  mutate(fsad_mad,  DVN = DV/DOSE)
fsims <- mutate(fsims, YN = Y/DOSE)

#' # Create the plot
#' 
#' Pass observed and simulated data into vpc function
p1 <- vpc(
  obs = fsad_mad,
  sim = fsims,
  stratify = "STUDY",
  obs_cols = list(dv = "DVN"),
  sim_cols=list(dv="YN", sim="irep"), 
  log_y = TRUE,
  pi = c(0.05, 0.95),
  ci = c(0.025, 0.975), 
  facet = "columns",
  show = list(obs_dv = TRUE), 
  vpc_theme = mrg_vpc_theme
) 

p1 <- 
  p1 +  
  theme_bw() + 
  xlab(lab$TIME) + 
  ylab("Dose-normalized concentration (ng/mL)")

p1

mrggsave_last(stem = "pk-vpc-{runno}-dose-norm", height = 7.5)

#' # Stratify on RF
rf_data <- filter(data, STUDYN==3) %>% select(-C,-USUBJID)

set.seed(54321)
rf_sims <- lapply(
  isim, sim, 
  data = rf_data, 
  mod = mod
) %>% bind_rows()

sum(rf_sims$Y)

#' Filter both the observed and simulated data
f_rf_data <- filter(rf_data, EVID==0, BLQ ==0)
f_rf_sims <- filter(rf_sims, EVID==0, Y >= 10)

f_rf_data <- mutate(f_rf_data, DVN = DV/DOSE)
f_rf_sims <- mutate(f_rf_sims, YN  = Y/DOSE)

p2 <- vpc(
  obs = f_rf_data,
  sim = f_rf_sims,
  stratify = "Renal",
  obs_cols = list(dv = "DVN"),
  sim_cols=list(dv="YN", sim="irep"), 
  log_y = TRUE,
  pi = c(0.05, 0.95),
  ci = c(0.025, 0.975), 
  show = list(obs_dv = TRUE), 
  vpc_theme = mrg_vpc_theme
) 

p2 <- 
  p2 +  
  theme_bw() + 
  xlab(lab$TIME) + 
  ylab("Dose-normalized concentration (ng/mL)")

p2

mrggsave_last(stem = "pk-vpc-{runno}-dose-norm-rf", height = 6.5)

#' # Stratify on CP
cp_data <- filter(data, STUDYN==4) %>% select(-C,-USUBJID)

set.seed(7456874)
cp_sims <- lapply(
  isim, 
  sim, 
  data = cp_data, 
  mod = mod
) %>% bind_rows()

sum(cp_sims$Y)

#' Filter both the observed and simulated data
f_cp_data <- filter(cp_data, EVID==0, BLQ ==0)
f_cp_sims <- filter(cp_sims, EVID==0, Y >= 10)

f_cp_data <- mutate(f_cp_data, DVN = DV/DOSE)
f_cp_sims <- mutate(f_cp_sims, YN  = Y/DOSE)

p3 <- vpc(
  obs = f_cp_data,
  sim = f_cp_sims,
  stratify = "Hepatic",
  obs_cols = list(dv = "DVN"),
  sim_cols=list(dv="YN", sim="irep"), 
  log_y = TRUE,
  pi = c(0.05, 0.95),
  ci = c(0.025, 0.975), 
  labeller = label_value,
  show = list(obs_dv = TRUE), 
  vpc_theme = mrg_vpc_theme
) 

p3 <- 
  p3 +  
  theme_bw() + 
  xlab(lab$TIME) + 
  ylab("Dose-normalized concentration (ng/mL)")

p3

mrggsave_last(stem = "pk-vpc-{runno}-dose-norm-cp", height = 6.5)

#' VPC for BLQ data. Note that the DV values in the observed dataset must be
#' something less than the lloq.

p4 <- vpc_cens(
  sim = sims, 
  obs = sad_mad, 
  lloq = 10,
  obs_cols = list(dv = "DV"),
  sim_cols = list(dv="Y", sim="irep")
)

p4

mrggsave_last(stem = "pk-vpc-{runno}-BLQ", height = 6.5)
