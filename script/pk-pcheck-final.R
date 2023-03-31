#' --- 
#' title: Predictive checks with mrgsolve
#' ---
#' 
#' # Scope
#' 
#' 1. Generate histogram-based predictive check on AUC for studies with single
#'    dose
#' 1. Generate QQ plot based predictive check on Cmin by renal function group
#' 

#' # Required packages
library(tidyverse)
theme_set(theme_bw())
library(yspec)
library(pmplots)
library(mrgsolve)
library(mrggsave)
library(future)
library(future.apply)
library(data.table)
library(mrgmisc)
library(bbr)
library(here)

options(mrggsave.dir = here("deliv/figure"), mrg.script = "pk-pcheck-final.R")

#' # Data
#' 
#' First, we have some observations that are BQL that need to be
#' dealt with.  We use a complete data approach, where when we 
#' simulate, we simulate the complete design or all intended samples 
#' and then apply any BLQ rules accordingly (e.g., drop values less than 
#' BLQ). 
#' 

#' 
#' The (full) analysis data set; we get this using the `.superset = TRUE` 
#' argument
data <- nm_join(here("model/pk/106"), .superset = TRUE)
data <- filter(data, is.na(C))

spec <- ys_load(here("data/spec/analysis3.yml"))
lab <- ys_get_short_unit(spec, parens = TRUE)

#' # Simulate the npc
#' 
#' ## Load the mrgsolve model
#' 
#' This should reflect `../model/pk/106.ctl`
mod <- mread("106.mod", project = here("script/model"))

#' # Set up the simulation
#' 
#' Create a function to simulate out one replicate
sim <- function(rep, data, model) {
  mrgsim(
    model, 
    data = data,
    carry_out = "EVID,STUDYN,LDOS,DOSE",
    recover = "STUDY,C,USUBJID,ACTARM,RF", 
    Req = "Y", 
    output = "df", 
    quiet  = TRUE
  ) %>%  mutate(irep = rep) 
}

#' Simluate data
options(future.fork.enable = TRUE)
plan(multicore, workers = 4)

#' 1000 replicates
nsim <- 1000

set.seed(54321)
sims <- future_lapply(
    seq_len(nsim), 
    sim, 
    data = data, 
    mod = mod,
    future.seed = TRUE
  ) %>% bind_rows()

#' Metrics for summaries
pct <- list(lo = 0.1, median = 0.5, hi = 0.9)
metrics <- names(pct)
labels <- c("10th percentile", "median", "90th percentile")

metricf <- function(x) {
  factor(fct_inorder(x),labels=labels)  
}

smrize <- function(x) {
  list(
    median = quantile(x,pct$med), 
    lo = quantile(x,pct$lo), 
    hi = quantile(x,pct$hi)
  )
}

#' # Whatever we do to the simulated data, we do to the observed data
#' 
#' First, summarize single doses in normal renal function
#' 
#' Filter:
single <- 
  sims %>% 
  filter(Y >= 10, STUDYN %in% c(1,4), TIME <= 96, EVID==0) %>% 
  as.data.table()

obs <- 
  data %>% 
  filter(BLQ ==0, STUDYN %in% c(1,4), TIME <= 96, EVID==0) %>% 
  as.data.table()

ggplot(obs, aes(TIME,DV,group=ID)) + geom_line() + facet_wrap(~STUDY)

#' Summarize observations
#' 
#' 1. Calculate AUC for each ID
#' 2. Summarize 10,50,90th percentiles
#' 3. Reshape and get ready to plot
auc_obs <- obs[,list(auc = auc_partial(TIME,DV/DOSE)), by="ID,STUDY,STUDYN"]
auc_obs_sum <- auc_obs[,smrize(auc), by = "STUDY,STUDYN"]
auc_obs_summ <- pivot_longer(as_tibble(auc_obs_sum), cols = all_of(metrics))
auc_obs_summ <- mutate(auc_obs_summ, name = metricf(name)) 

#' Now do it on the simulated data
auc_sim <- single[,list(auc = auc_partial(TIME,Y/DOSE)),by="ID,STUDY,STUDYN,irep"]
auc_sim_sum <- auc_sim[,smrize(auc), by = "STUDY,STUDYN,irep"]
auc_sims <- pivot_longer(auc_sim_sum, cols = all_of(metrics))
auc_sims <- mutate(auc_sims, name = metricf(name))

#' Plot
cont_hist(auc_sims, x = "value", bins = 23, alpha = 0.5) + 
  facet_grid(STUDY~name, scales = "free_x") + 
  geom_vline(data = auc_obs_summ, aes(xintercept = value), col = "blue3", linewidth = 1.5) + 
  xlab("Dose-normalized AUC (ng*hr/ml/mg)")

mrggsave_last(tag = "auc-pcheck", height = 6)

#' # QQ plot
#' 
#' - Again: the principle is that we do the same thing to the simulated data 
#'   replicates that we do to the observed data
#' 
#' 1. Calculate dose-normalized Cmin over the first 12 hours
#' 
sims_cmin <- filter(sims, STUDYN  %in% c(2,3), TIME > 116, TIME <= 130, EVID==0)
obs_cmin  <- filter(data, STUDYN  %in% c(2,3), TIME > 116, TIME <= 130, EVID==0)

#' Censor 
sims_cmin <- mutate(sims_cmin, Y = ifelse(Y < 10, NA_real_, Y))
obs_cmin <- mutate(obs_cmin, DV = ifelse(BLQ == 1, NA_real_, DV))

#' Get the dose-normalized observed Cmin
omin <- 
  obs_cmin %>%
  filter(!is.na(DV)) %>% # drop?
  group_by(ID,DOSE,RF) %>%
  summarise(Cmin_obs = min(DV/DOSE), .groups = "drop")

#' Get the dose-normalized simulated Cmin
smin <- 
  sims_cmin %>%
  filter(!is.na(Y)) %>% # drop?
  group_by(irep,ID,DOSE,RF) %>%
  summarise(Cmin = min(Y/DOSE), .groups = "drop") 

#' Join the observed values onto the (replicate) simulated values  
cmin <- left_join(smin, omin)

#' Now, we sort the simulated ans observed values within replicate number and
#' the stratification variable
cmin_sort <- 
  cmin %>%
  group_by(irep, RF) %>% 
  mutate(Cmin_obs = sort(Cmin_obs), Cmin = sort(Cmin))

#' Factorize
cmin_sort <- ys_add_factors(cmin_sort, spec, RF, .suffix = "")

spikes <- 
  omin %>% 
  ys_add_factors(spec, RF, .suffix = "") %>%
  mutate(DV = Cmin_obs) %>%
  group_by(RF) %>% 
  summarise(
    med = median(DV), lower = quantile(DV,0.1), 
    upper = quantile(DV,0.9), .groups = "drop"
  ) 

spikes_long <- pivot_longer(spikes, cols = c("med", "lower", "upper"))

#' - reference lines for 25th/50th/75th percentiles
#' Plot
ggplot(cmin_sort, aes(Cmin_obs,Cmin,group=irep)) + 
  geom_line(col = "darkgrey", alpha  = 1.2) + 
  facet_wrap(~RF, scales = "free") + 
  geom_vline(data = spikes_long, aes(xintercept = value, group = name),lty=2) + 
  xlab("Observed dose-normalized Cmin (ng/mL/mg)") + 
  ylab("Simulated dose-normalized Cmin (ng/mL/mg)") + 
  geom_abline(intercept=0, slope=1, linewidth=1.5, col = "blue3")

mrggsave_last(tag = "cmin-pcheck", height = 6)
