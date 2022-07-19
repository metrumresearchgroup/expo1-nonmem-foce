#' Purpose: 
#' - Read in EBE from final model run 106
#' - Simulate each individual to steady state at the first dose in the data
#'   set
#' - Summarize ss Cmax, Cmin, and AUC

library(tidyverse)
library(mrgsolve)
library(bbr)
library(PKPDmisc)
library(pmtables)
library(data.table)
library(yspec)
library(here)

options(mrgsolve.soloc = here("script/build"))
options(bbr.verbose = FALSE, pmtables.dir = here("deliv/table"))

mod <- modlib("pk2")

spec <- ys_load(here("data/spec/analysis3.yml"))

data <- nm_join(here("model/pk/106"))

dose_rec <- filter(data, EVID==1)

pars <- distinct(dose_rec,ID,CL,V2,Q,V3,KA,AMT,RF,ACTARM)

inventory(mod, pars)

dose <- mutate(pars, II=24, SS=1, TIME=0, EVID=1, CMT=1)

dose <- mutate(dose, DOSE = AMT)

mod <- update(mod, delta = 0.1, end = 24)

out <- mrgsim_df(mod, dose, recover = "ACTARM,RF,CL,DOSE", recsort = 3)

maxmin <- 
  out %>% 
  group_by(ID,DOSE) %>% 
  summarise(Cmax = max(CP), Cmin = min(CP), .groups = "drop")

auc <- 
  out %>% 
  mutate(.auc = DOSE/CL) %>% 
  group_by(ID,DOSE,.auc) %>% 
  summarise(AUC = auc_partial(TIME,CP), .groups = "drop")

pk <- left_join(maxmin, auc, by = c("ID", "DOSE"))

pk <- mutate(pk, .auc = NULL)

id <- distinct(data, ID, USUBJID, STUDY, RF)

pk <- left_join(pk, id, by = "ID")

pk <- select(pk, STUDY,  USUBJID, everything()) 

normal <- filter(pk, RF=="norm")
normal <- ys_add_factors(normal, spec, .suffix = "f")

pmtab <- pt_cont_long(
  normal, 
  cols = "AUC, Cmax, Cmin", 
  panel = vars("Daily dose:" = "DOSEf"), 
  summarize_all = FALSE
)
notes <- c(
  "AUC: ng$\\cdot$hr/mL, Cmax: ng/mL, Cmin: ng/mL",
  "All exposure metrics at steady state",
  "N: number of individuals"
)
tab <- stable(
  pmtab, 
  notes = notes, 
  r_file = "pk-ebe-simulate.R", 
  output_file = "pk-ebe-normal.tex", 
  sizes = tab_size(row = 1.1)
)

# st2viewer(tab)
stable_save(tab) 

#' # Compare normal to severe renal groups
#' - Doses between 25 and 75 mg daily
pk_long <- pivot_longer(pk, cols = c(Cmax, Cmin, AUC))
fwrite(pk_long, file = here("data/derived/pk-ebe-simulate.csv"), na = '.')

pk_sum <- 
  pk_long %>% 
  group_by(DOSE, RF, Metric = name) %>% 
  summarise(
    N = n(),
    Median = median(value), 
    Min = min(value), 
    Max = max(value),
    .groups = "drop"
  )

compare <- filter(pk_sum, DOSE >= 25 & DOSE <= 75)
pk_sum <- mutate(compare, across(c(Median, Min, Max), sig))
compare <- mutate(compare, DOSEf = paste0(DOSE, " mg daily")) 

compare <- ys_add_factors(compare, spec, RF, .suffix = "f")
compare <- select(compare, DOSE, Metric, Renal = RFf, everything())

compare <- arrange(compare, DOSE, Metric, Renal)
compare <- mutate(compare, RF = NULL, DOSE = NULL, Renal = as.character(Renal))

compare <- mutate(compare, across(c(Median, Min, Max), sig))

tab2 <- 
  compare %>% 
  st_new() %>%
  st_right(.l = "Metric, Renal") %>% 
  st_clear_reps(Metric) %>%
  st_panel("DOSEf", prefix = "Dose:") %>%
  st_notes("AUC: ng$\\cdot$hr/mL, Cmax: ng/mL, Cmin: ng/mL") %>% 
  st_notes("All exposure metrics at steady state") %>% 
  st_notes("N: number of individuals") %>% 
  st_files(r = "pk-ebe-simulate.R", output = "pk-ebe-compare.tex") %>% 
  stable()

# st_preview(tab2) 
stable_save(tab2) 

if(FALSE) { # Optional preview as it would appear in a report
  st2report(list(tab, tab2), ntex = 2)
}
