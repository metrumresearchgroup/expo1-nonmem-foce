# Notes -------------------------------------------------------------------
# Additional examples and notes can be found on the KB:
# https://ghe.metrumrg.com/pages/mrg/knowledgebase/Science_Resources/example_projects/NONMEM_PopPK_FOCE/forest_plots/

#' Create covariate forest plot for CL 
#' Covariates: WT, EGFR, ALB, AGE
#' We will re-express EGFR in terms of renal function stages (e.g. moderate)

#' Required packages
library(tidyverse)
library(mrgsolve)
library(here)
library(data.table)
library(yspec)
library(rlang)
library(bbr)

#' Load data specification object and modify
spec <- ys_load(here("data/spec/analysis3.yml"))

spec <- update_short(spec, EGFR = "eGFR")

spec <- yspec:::ys_mutate(
  spec, 
  EGFR = list(short = "eGFR", unit = NULL)
)

unit <- ys_get_unit(spec) %>% 
  as_tibble() %>% 
  pivot_longer(everything(), values_to = "unit")

#' Load the bootstrap parameter estimates from run 106 and take a subset
post <- fread(here("data/boot/boot-106.csv"))
post <- select(post, contains("THETA")) %>% slice_sample(n = 500) 
post <- mutate(post, iter = row_number())

#' Load the mrgsolve model
mod <- mread(here("script/model/106.mod"), capture = "CL")
mod <- update(mod, end = -1) %>% zero_re()

#' Function to simulate CL
simulate_for_forest <- function(values, col) {
  #' Make an idata set
  idata <- tibble(!!sym(col) := values, LVL = seq_along(values))
  idata <- crossing(post, idata)
  #' Simulate
  out <-
    mod %>% 
    mrgsim_i(idata, carry_out = c(col, "LVL", "iter"), output = "df") 
  #' Groom the output
  out <- 
    out %>% 
    mutate(name = col, value = !!sym(col)) %>% 
    select(-!!sym(col)) %>%
    arrange(LVL)
  
  #' Process renames
  if(is_named(values)) {
    out <- mutate(
      out, 
      value := factor(value, labels = names(values), levels = values)
    )
  } else {
    out <- mutate(out, value = fct_inorder(as.character(value)))  
  }
  out
}

#' Set up scenarios 
#' This should reflect the order of the covariates and the order of 
#' covariate levels
#' These levels are levels of interest and typically chosen to cover the 
#' distribution of covariates in the data set
data <- bbr::nm_join(here("model/pk/106"))
data %>% 
  distinct(ID, EGFR, ALB, AGE, WT) %>% 
  pivot_longer(-ID) %>% 
  group_by(name) %>% 
  summarise(as_tibble_row(quantile(value, c(0.1, 0.5, 0.9))))

#' We'll use these values
x <- list(
  WT = c(85, 55), 
  EGFR = c(Severe = 22.6, Moderate = 42.9, Mild = 74.9), 
  ALB = c(5, 3.25), 
  AGE = c(45, 20)
)

#' Simulate the reference
ref <- mrgsim(mod, idata = post) %>% select(iter = ID, ref = CL)

#' Simulate the scenarios
out <- imap_dfr(x, simulate_for_forest)

#' Grooming
out <- left_join(out, unit, by = "name")
out <- mutate(out, cov_level = paste(value, unit))
out <- left_join(out, ref)

#' We want to plot the clearance relative to reference
out <- mutate(out, relcl = CL/ref)


#' Set up labels for the plot
all_labels <- ys_get_short(spec, title_case = TRUE)
all_labels$EGFR <- 'Renal Function'
all_labels$CL <- 'CL (L/hr)'
plot_labels <- as_labeller(unlist(all_labels))

#' Summarize the simulated clearances
sum_data <- pmforest::summarize_data(
  data = out , 
  value = "relcl",
  group = "name",
  group_level = "cov_level",
  probs = c(0.025, 0.975),
  statistic = "median"
)

#' Plot
clp <- pmforest::plot_forest(
  data = sum_data,
  summary_label = plot_labels,
  text_size = 3.5,
  shape_size = 2.5,
  shapes = "circle",
  vline_intercept = 1,
  x_lab = "Fraction and 95% CI \nRelative to Reference",
  CI_label = "Median [95% CI]",
  plot_width = 8, 
  x_breaks = c(0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6), 
  x_limit = c(0.4, 1.45),
  annotate_CI = TRUE,
  nrow = 1
) 

clp
