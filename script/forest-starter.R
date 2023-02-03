library(pmforest)
library(dplyr)
library(mrgsolve)
library(data.table)
library(tidyr)
library(forcats)
library(here)

set.seed(11010)

nfact <- function(x, suffix = "", prefix = "") {
  ux <- sort(unique(x))
  factor(x, levels = ux, labels = paste0(prefix, ux, suffix))
}

rff <- function(x) {
  factor(
    x, 
    levels = c("norm", "mild", "mod", "sev"), 
    labels = c("Normal function", "Mild impairment", 
               "Moderate impairment", "Severe impairment"
    )
  )
  
}

dd <- expand.grid(
  dose = c(10, 20, 30)
)

covar <- fread(here("data/derived/analysis3.csv"), na.strings = '.')
covar <- filter(covar, STUDYN %in% c(1,3))
covar <- distinct(covar, ID, .keep_all=TRUE)
covar <- group_by(covar, RF)
covar <- slice_sample(covar, n = 3000, replace = TRUE)
covar <- ungroup(covar)
covar <- mutate(covar, ID = row_number())
lev <- crossing(dd, covar)

data <- mutate(lev, ID = row_number())

data <- mutate(
  data, 
  dose2 = case_when(
    RF=="sev" ~ dose * 0.5, 
    RF=="mod" ~ dose * 0.7,
    TRUE ~ dose
  ), 
  dose1 = dose
)

mod <- mread("106.mod", project = here("script/model"))
see(mod)
out <- mrgsim(mod, data, end = -1, recover = "dose1,dose2,RF,EGFR", output = "df")

out1 <- mutate(
  out, 
  AUC = dose1/CL, 
  dose = dose1, 
  type = "original"
)
out2 <- mutate(
  out, 
  AUC = dose2/CL,
  dose = dose2, 
  type = "adjusted"
)

out <- bind_rows(out1, out2)
out <- mutate(
  out, 
  RFf = rff(RF), 
  RF = as.integer(RFf),
  dosef = nfact(dose, " mg")
)
out <- as_tibble(out)

head(out)

out <- select(
  out, 
  ID, RF, RFf, 
  dose, dosef, 
  type,
  CL, AUC
)
out <- arrange(out, RFf, dose, dosef)

fwrite(
  out, 
  file = here("data/derived/forest-starter.csv")
)

out <- fread(file = here("data/derived/forest-starter.csv"))

out <- mutate(
  out, 
  type = fct_inorder(type),
  RFf = fct_reorder(RFf, RF), 
  dosef = fct_reorder(dosef, dose)
)


sims1 <- filter(out, type=="adjusted")
summ1 <- summarize_data(
  sims1, 
  value = "AUC", 
  group = "RFf", 
  group_level = "dosef"
)

plot_forest(
  summ1,
  x_limit = c(0, 41), 
  x_lab = "AUCss (ng*hr/mL)", 
  CI_label = "Exposure (AUCss)"
)


sims2 <- filter(out, type == "original")
summ2 <- summarize_data(
  sims2, 
  value = "AUC", 
  group = "dosef", 
  group_level = "RFf"
)

plot_forest(
  summ2,
  x_limit = c(0, 41), 
  x_lab = "AUCss (ng*hr/mL)", 
  CI_label = "Exposure (AUCss)"
)

summ3 <- summarize_data(
  out, 
  value = "AUC", 
  group = "RFf", 
  group_level = "dosef", 
  metagroup = "type"
)

plot_forest(
  summ3,
  x_limit = c(0, 41), 
  x_lab = "AUCss (ng*hr/mL)", 
  CI_label =  " ", 
  shape_size = 2, 
  ncol = 1
)

