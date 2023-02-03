library(tidyverse)
theme_set(theme_bw())
library(mrgsolve)
library(data.table)
library(haven)
library(mrgmisc)
library(pmplots)
library(pmforest)
library(mrggsave)
library(nhanesA)
library(here)
library(future)
library(furrr)

setwd(here("script"))
out_data_dir <- here("data", "sim")

# Set to TRUE to re-run all simulations
# If FALSE, will load data from previous simulations
# NOTE: if running sims, use a large head node (ideally 16 vCPU's)
RUN_SIMS <- FALSE

options(
  mrgsolve.project = "model", 
  mrggsave.dir = "../deliv/figure", 
  mrg.script = "pk-sim-renal.R",
  dplyr.summarise.inform = FALSE,
  tibble.width = Inf
)

plan(multisession)
set.seed(20205432)
opt <- furrr_options(seed = TRUE)

#Read in bootstrap (or posterior or simpar and covariance matrix)
boot <- fread(here("data", "boot", "boot-106.csv"))
boot <- mutate(boot, ID = seq(n()))

#Read in NHANES datasets
letters <- c('',paste("_",LETTERS[2:10],sep=''))

dem <- map_df(letters,.f = function(x){
  nhanes(paste('DEMO',x,sep='')) %>% 
    select(ID = SEQN, AGE = RIDAGEYR, 
           SEX = RIAGENDR, RACE = RIDRETH1) %>%
    mutate_all(.funs = as.numeric)})

demo_labels <- tibble(
  col = names(dem),
  label = sapply(dem, attr, "label")
)

wt <- map_df(letters,.f = function(x){
  nhanes(paste('BMX',x,sep='')) %>% 
    select(ID = SEQN, WT = BMXWT) %>%
    mutate_all(.funs = as.numeric)})

bmx_labels <- tibble(
  col = names(wt),
  label = sapply(wt, attr, "label")
)

lab <- map_df(letters,.f = function(x){
  return(x %>% purrr::when(. =='' ~ nhanes(paste('LAB18',sep='')) %>% 
                             select(ID = SEQN, ALB = LBXSAL, SCR = LBXSCR) %>%
                             mutate_all(.funs = as.numeric),
                           . == '_B' ~ nhanes(paste('L40',.,sep='')) %>% 
                             select(ID = SEQN, ALB = LBXSAL, SCR = LBDSCR) %>%
                             mutate_all(.funs = as.numeric),
                           . == '_C' ~ nhanes(paste('L40',.,sep='')) %>% 
                             select(ID = SEQN, ALB = LBXSAL, SCR = LBXSCR) %>%
                             mutate_all(.funs = as.numeric),
                           . %in% letters[4:10] ~ nhanes(paste('BIOPRO',.,sep='')) %>% 
                             select(ID = SEQN, ALB = LBXSAL, SCR = LBXSCR) %>%
                             mutate_all(.funs = as.numeric),
                           ~ stop("No matches found: check data name")))})

lab_labels <- tibble(
  col = names(lab), 
  label = sapply(lab, attr, "label")
)


#Join into 1 dataset and generate EGFR
covar <- left_join(dem, lab) %>% left_join(wt)
anyNA(covar)
# TRUE
covar <- filter(covar, !is.na(WT) & !is.na(ALB) & !is.na(SCR))
anyNA(covar)
# FALSE

# https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.htm#RIDRETH1 BLACK
# https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.htm#RIAGENDR FEMALE
covar <- mutate(
  covar %>% filter(AGE>=18), 
  EGFR = 175 * (SCR)^(-1.154) * AGE^(-0.203) * 0.742^(SEX==2) * 1.212^(RACE==4), 
  USUBJID = ID, 
  ID = seq(n())
)

#Create disease variable
pdisease <- function(age, e0 = 0.0, emax = 0.5, ea50 = 30) {
  p <- e0 + (emax - e0)*age/(age + ea50)
  return(p)
}

#Create mock data set with enrolled and non-enrolled subjects
set.seed(06242021)
covar2 <- covar %>%
  mutate(Condition = if_else(as.logical(rbinom(n(), 1, pdisease(AGE))), 
                             "Diseased", "Healthy"),
         Condition = factor(Condition, levels = c("Healthy", "Diseased"))
  )

studyA <- covar2 %>%
  filter(Condition == "Healthy" & AGE <= 65 & EGFR > 90) %>%
  slice_sample(n = 1000) %>%
  mutate(STUDY = "A")

studyB <- covar2 %>%
  filter(Condition == "Healthy" & AGE > 60 & EGFR > 60) %>%
  slice_sample(n = 500) %>%
  mutate(STUDY = "B")

simdat_enrolled <-
  covar2 %>%
  anti_join(studyA, by = "USUBJID") %>%
  bind_rows(studyA) %>%
  anti_join(studyB, by = "USUBJID") %>%
  bind_rows(studyB) %>%
  mutate(STUDY = fct_explicit_na(STUDY, na_level = "Not Enrolled"),
         STUDY_COND = paste0(STUDY, " (", as.character(Condition), ")"),
         STUDY_COND = gsub("Not Enrolled \\(Diseased\\)", 
                           "Target Population (Diseased)", STUDY_COND),
         STUDY_COND = factor(STUDY_COND, levels = c("A (Healthy)", 
                                                    "B (Healthy)", 
                                                    "Not Enrolled (Healthy)",
                                                    "Target Population (Diseased)")))

#Distribution of enrolled and disease status
simdat_enrolled %>%
  filter(STUDY %in% c('A','B')) %>%
  bind_rows(simdat_enrolled %>% filter(STUDY == 'Not Enrolled') %>%
              sample_n(2500)) %>%
  filter(AGE < 80,EGFR<=150) %>%
  mutate(STUDY = STUDY_COND) %>%
  arrange(Condition) %>%
  ggplot() + # just making sure I know what EGFR I'm dealing with
  geom_point(aes(x = AGE, y = EGFR, shape = STUDY, colour = STUDY)) +
  scale_color_manual(values = c("turquoise3", "tomato1", "steelblue", "black")) +
  scale_shape_manual(values = c(1, 1, 2, 1)) +
  geom_hline(yintercept = c(90, 60), linetype = 2, color = 'firebrick',
             lwd = 1.5) +
  geom_vline(xintercept = 60, linetype = 2, color = 'firebrick',
             lwd = 1.5)

mrggsave_last(stem = "pk-sim-renal-enrolled-disease-distribution", height = 4, width = 6)

#Prevalence of MRG-itis
simdat_enrolled %>%
  ggplot() +
  geom_density(aes(x = AGE, colour = Condition)) + 
  labs(x = 'Age (y)', y = 'Density') + 
  scale_color_discrete(labels = c('Healthy', 'MRG-itis'))

#Generate AUC distribution for observed data: Studies A and B
#Generate AUC distribution for populations with characteristics of interest
## Target population; 
## Target with subset of age (>65 yr); 
## Target with subset of age and egfr (>65 yr; EGFR > & > ?)
set.seed(062421)
obsdat <- simdat_enrolled %>%
  filter(STUDY %in% c('A','B')) %>%
  mutate(GRP = 'Observed')
target <- simdat_enrolled %>%
  filter(STUDY_COND == 'Target Population (Diseased)') %>%
  sample_n(size = 500,replace = T) %>%
  mutate(GRP = 'Target Population')
target_eld <- simdat_enrolled %>%
  filter(STUDY_COND == 'Target Population (Diseased)',
         AGE > 65) %>%
  sample_n(size = 500,replace = T) %>%
  mutate(GRP = 'Target Elderly')
target_eld_ren1 <- simdat_enrolled %>%
  filter(STUDY_COND == 'Target Population (Diseased)',
         AGE > 65, EGFR>90) %>%
  sample_n(size = 500,replace = T) %>%
  mutate(GRP = 'Target Elderly; Normal Renal')
target_eld_ren2 <- simdat_enrolled %>%
  filter(STUDY_COND == 'Target Population (Diseased)',
         AGE > 65, EGFR<90 & EGFR >59) %>%
  sample_n(size = 500,replace = T)%>%
  mutate(GRP = 'Target Elderly; Mild Renal Imp')
target_eld_ren3a <- simdat_enrolled %>%
  filter(STUDY_COND == 'Target Population (Diseased)',
         AGE > 65, EGFR<60 & EGFR >44) %>%
  sample_n(size = 500,replace = T) %>%
  mutate(GRP = 'Target Elderly; 3a Mod Renal Imp')
target_eld_ren3b <- simdat_enrolled %>%
  filter(STUDY_COND == 'Target Population (Diseased)',
         AGE > 65, EGFR<45 & EGFR >29) %>%
  sample_n(size = 500,replace = T) %>%
  mutate(GRP = 'Target Elderly; 3b Mod Renal Imp')
target_eld_ren4 <- simdat_enrolled %>%
  filter(STUDY_COND == 'Target Population (Diseased)',
         AGE > 65, EGFR<30 & EGFR >14) %>%
  sample_n(size = 500,replace = T)%>%
  mutate(GRP = 'Target Elderly; Severe Renal Imp')

#Stack the datasets and make a new ID variable
simdat_tot <- bind_rows(obsdat,target,target_eld,
                        target_eld_ren1,target_eld_ren2,
                        target_eld_ren3a,target_eld_ren3b,
                        target_eld_ren4) %>%
  mutate(ID = seq(n()))

#Get all non-healthy subjects
covar <- filter(covar, EGFR < 90, AGE >= 18)
covar <- sample_n(covar, nrow(covar)) %>% mutate(ID = seq(n()))

if (isTRUE(RUN_SIMS)) {
  #Read in mrgsolve model
  mod <- mread("106.mod") %>% zero_re('sigma')
  param(mod)
  omat(mod)
  inventory(mod, simdat_tot)
  
  simt <- tgrid(start=0,
                end = 24,
                delta = 0.1)
  
  ddat <- simdat_tot %>% mutate(time=0,
                                amt=100, evid=1,cmt=1,ss=1, ii=24)
  
  #Let's pretend we have EBEs for studies A and B
  #This will be our "reference"
  set.seed(05302021)
  simresAB <- mod %>%
    data_set(ddat %>% filter(STUDY %in% c('A','B'))) %>%
    mrgsim_df(obsonly=TRUE, tgrid=simt, 
              recover = 'GRP') %>%
    filter(time!=0 & GUT!=0) %>%
    group_by(ID,GRP) %>%
    summarize(AUCSS = auc_partial(time,IPRED),
              CMINSS = min(IPRED),
              CMAXSS = max(IPRED)) %>%
    ungroup() 
  
  simres2AB <- simresAB %>% 
    group_by(GRP) %>%
    summarize(med1 = median(AUCSS),
              lo1 = quantile(AUCSS, prob=0.05),
              hi1 = quantile(AUCSS, prob=0.95),
              med2 = median(CMINSS),
              lo2 = quantile(CMINSS, prob=0.05),
              hi2 = quantile(CMINSS, prob=0.95),
              med3 = median(CMAXSS),
              lo3 = quantile(CMAXSS, prob=0.05),
              hi3 = quantile(CMAXSS, prob=0.95)) %>%
    ungroup() %>%
    mutate(GRP = factor(GRP,
                        levels = c('Observed',
                                   'Target Population',
                                   'Target Elderly',
                                   'Target Elderly; Normal Renal',
                                   'Target Elderly; Mild Renal Imp',
                                   'Target Elderly; 3a Mod Renal Imp',
                                   'Target Elderly; 3b Mod Renal Imp',
                                   'Target Elderly; Severe Renal Imp')))
  
  #Iteratively resample populations and simulate with uncertainty
  set.seed(05302021)
  simresIIVJul17 <- future_map(
    mrgmisc::chunk_df(boot, run, .nchunks = 16),.options = opt,
    function(.chunk){
      map_dfr(
        .chunk$run, 
        function(iter, params = .chunk, nhanesdf = simdat_enrolled,
                 simmod = mod) {
          
          uc <- filter(params, run==iter) %>% select(-ID)
          
          #Generate AUC distribution for populations with characteristics of interest
          
          target <- nhanesdf %>%
            filter(STUDY_COND == 'Target Population (Diseased)') %>%
            sample_n(size = 5000,replace = T) %>%
            mutate(GRP = 'Target Population')
          target_eld <- nhanesdf %>%
            filter(STUDY_COND == 'Target Population (Diseased)',
                   AGE > 65) %>%
            sample_n(size = 5000,replace = T) %>%
            mutate(GRP = 'Target Elderly')
          target_eld_ren1 <- nhanesdf %>%
            filter(STUDY_COND == 'Target Population (Diseased)',
                   AGE > 65, EGFR>90) %>%
            sample_n(size = 5000,replace = T) %>%
            mutate(GRP = 'Target Elderly; Normal Renal')
          target_eld_ren2 <- nhanesdf %>%
            filter(STUDY_COND == 'Target Population (Diseased)',
                   AGE > 65, EGFR<90 & EGFR >59) %>%
            sample_n(size = 5000,replace = T)%>%
            mutate(GRP = 'Target Elderly; Mild Renal Imp')
          target_eld_ren3a <- nhanesdf %>%
            filter(STUDY_COND == 'Target Population (Diseased)',
                   AGE > 65, EGFR<60 & EGFR >44) %>%
            sample_n(size = 5000,replace = T) %>%
            mutate(GRP = 'Target Elderly; 3a Mod Renal Imp')
          target_eld_ren3b <- nhanesdf %>%
            filter(STUDY_COND == 'Target Population (Diseased)',
                   AGE > 65, EGFR<45 & EGFR >29) %>%
            sample_n(size = 5000,replace = T) %>%
            mutate(GRP = 'Target Elderly; 3b Mod Renal Imp')
          target_eld_ren4 <- nhanesdf %>%
            filter(STUDY_COND == 'Target Population (Diseased)',
                   AGE > 65, EGFR<30 & EGFR >14) %>%
            sample_n(size = 5000,replace = T)%>%
            mutate(GRP = 'Target Elderly; Severe Renal Imp')
          
          #Stack the datasets and make a new ID variable
          simdat_tot <- bind_rows(target,target_eld,
                                  target_eld_ren1,target_eld_ren2,
                                  target_eld_ren3a,target_eld_ren3b,
                                  target_eld_ren4) %>%
            mutate(ID = seq(n()))
          
          
          simt <- tgrid(start=0,
                        end = 24,
                        delta = 0.1)
          
          ddat <- simdat_tot %>% mutate(time=0,
                                        amt=100, evid=1,cmt=1,ss=1, ii=24)
          
          out <- mod %>%
            data_set(ddat) %>%
            param(uc %>% select(starts_with('THETA'))) %>%
            omat(uc %>% as_bmat('OMEGA')) %>%
            mrgsim_df(obsonly=TRUE, tgrid=simt, 
                      recover = 'GRP') %>%
            filter(time!=0 & GUT!=0) %>%
            group_by(ID,GRP) %>%
            summarize(AUCSS = auc_partial(time,IPRED),
                      CMINSS = min(IPRED),
                      CMAXSS = max(IPRED)) %>%
            ungroup() %>%
            mutate(run = iter)
          
          
          return(out)
          
        }
      )
    }
  )
  
  saveRDS(simresIIVJul17, file = file.path(out_data_dir, 'simresIIVPopJul17N5000.RDS'))
  saveRDS(simresAB, file = file.path(out_data_dir, 'simresAB.RDS'))  
  saveRDS(simres2AB, file = file.path(out_data_dir, 'simres2AB.RDS'))  
} else {
  simresIIVJul17 <- readRDS(file = file.path(out_data_dir, 'simresIIVPopJul17N5000.RDS'))
  simresAB <- readRDS(file = file.path(out_data_dir, 'simresAB.RDS'))
  simres2AB <- readRDS(file = file.path(out_data_dir, 'simres2AB.RDS')) 
}


#Create Normalized variables
simresIIV2 <- bind_rows(simresIIVJul17) %>% 
  bind_rows(simresAB) %>%
  mutate(GRP = factor(GRP,
                      levels = c('Observed',
                                 'Target Population',
                                 'Target Elderly',
                                 'Target Elderly; Normal Renal',
                                 'Target Elderly; Mild Renal Imp',
                                 'Target Elderly; 3a Mod Renal Imp',
                                 'Target Elderly; 3b Mod Renal Imp',
                                 'Target Elderly; Severe Renal Imp',
                                 'Target Elderly; Severe Renal Imp Adjusted Dose'))) %>%
  arrange(GRP) %>%
  mutate(N_AUCSS = AUCSS/simres2AB$med1,
         N_CMAXSS = CMAXSS/simres2AB$med2,
         N_CMINSS = CMINSS/simres2AB$med3)



# The code below (lines 375-475) is not needed ----------------------------
# scroll to bottom for other plots


#Summarize by run and GRP
sumIIV <- simresIIV2 %>%
  group_by(run, GRP) %>%
  summarize(loAUCSS = quantile(AUCSS,prob=0.05),
            MAUCSS = median(AUCSS),
            hiAUCSS = quantile(AUCSS,prob=0.95),
            loCMAXSS = quantile(CMAXSS,prob=0.05),
            MCMAXSS = median(CMAXSS),
            hiCMAXSS = quantile(CMAXSS,prob=0.95),
            loCMINSS = quantile(CMINSS,prob=0.05),
            MCMINSS = median(CMINSS),
            hiCMINSS = quantile(CMINSS,prob=0.95),
            loNAUCSS = quantile(N_AUCSS,prob=0.05),
            MNAUCSS = median(N_AUCSS),
            hiNAUCSS = quantile(N_AUCSS,prob=0.95),
            loNCMAXSS = quantile(N_CMAXSS,prob=0.05),
            MNCMAXSS = median(N_CMAXSS),
            hiNCMAXSS = quantile(N_CMAXSS,prob=0.95),
            loNCMINSS = quantile(N_CMINSS,prob=0.05),
            MNCMINSS = median(N_CMINSS),
            hiNCMINSS = quantile(N_CMINSS,prob=0.95)) %>%
  ungroup() %>%
  mutate(GRP = factor(GRP,
                      levels = c('Observed',
                                 'Target Population',
                                 'Target Elderly',
                                 'Target Elderly; Normal Renal',
                                 'Target Elderly; Mild Renal Imp',
                                 'Target Elderly; 3a Mod Renal Imp',
                                 'Target Elderly; 3b Mod Renal Imp',
                                 'Target Elderly; Severe Renal Imp',
                                 'Target Elderly; Severe Renal Imp Adjusted Dose')))
#Summarize across runs
sumIIV2med <- sumIIV %>%
  group_by(GRP) %>%
  mutate_at(.vars = vars(loAUCSS:hiNCMINSS), median) %>%
  select(-run) %>%
  distinct() %>%
  pivot_longer(loAUCSS:hiNCMINSS) %>%
  mutate(name = paste('Med_',name,sep=''))

sumIIV2lo <- sumIIV %>%
  group_by(GRP) %>%
  mutate_at(.vars = vars(loAUCSS:hiNCMINSS), .funs = function(x) quantile(x, prob=0.05)) %>%
  select(-run) %>%
  distinct() %>%
  pivot_longer(loAUCSS:hiNCMINSS) %>%
  mutate(name = paste('Lo_',name,sep=''))

sumIIV2hi <- sumIIV %>%
  group_by(GRP) %>%
  mutate_at(.vars = vars(loAUCSS:hiNCMINSS), .funs = function(x) quantile(x, prob=0.95)) %>%
  select(-run) %>%
  distinct() %>%
  pivot_longer(loAUCSS:hiNCMINSS) %>%
  mutate(name = paste('Hi_',name,sep=''))

#Stack them up
sumIIV2 <- bind_rows(sumIIV2med, sumIIV2lo, sumIIV2hi)

#Create plot df with all variables and groups
pltdf <- sumIIV2 %>%
  pivot_wider(id_cols = GRP, names_from = name,
              values_from = value) %>%
  bind_rows(simres2AB %>%
              mutate(Nmed1 = med1/med1,
                     Nlo1 = lo1/med1,
                     Nhi1 = hi1/med1,
                     Nmed2 = med2/med2,
                     Nlo2 = lo2/med2,
                     Nhi2 = hi2/med2,
                     Nmed3 = med3/med3,
                     Nlo3 = lo3/med3,
                     Nhi3 = hi3/med3)) %>%
  mutate(GRP = factor(GRP,
                      levels = c('Observed',
                                 'Target Population',
                                 'Target Elderly',
                                 'Target Elderly; Normal Renal',
                                 'Target Elderly; Mild Renal Imp',
                                 'Target Elderly; 3a Mod Renal Imp',
                                 'Target Elderly; 3b Mod Renal Imp',
                                 'Target Elderly; Severe Renal Imp',
                                 'Target Elderly; Severe Renal Imp Adjusted Dose')))

pltdf <- pltdf %>%
  bind_rows(pltdf %>%
              filter(GRP == 'Target Elderly; Severe Renal Imp') %>%
              mutate(GRP = 'Target Elderly; Severe Renal Imp Adjusted Dose') %>%
              mutate_each(funs = function(x) x/2 * (1 + rnorm(1,0,0.05)))) %>%
  mutate(GRP = factor(GRP,
                      levels = c('Observed',
                                 'Target Population',
                                 'Target Elderly',
                                 'Target Elderly; Normal Renal',
                                 'Target Elderly; Mild Renal Imp',
                                 'Target Elderly; 3a Mod Renal Imp',
                                 'Target Elderly; 3b Mod Renal Imp',
                                 'Target Elderly; Severe Renal Imp',
                                 'Target Elderly; Severe Renal Imp Adjusted Dose')))


data_auc_n <- simresIIV2 %>% 
  select(-c(AUCSS, CMINSS, CMAXSS)) %>% 
  pivot_longer(N_AUCSS:N_CMINSS) %>%
  subset(name=="N_AUCSS")

shaded_interval <- quantile(data_auc_n$value[data_auc_n$GRP=="Observed"], c(0.05,0.95)) 

sum_data <- summarize_data(
  data = data_auc_n %>% dplyr::filter(GRP != "Observed"), 
  value = value,
  group = GRP,
  #group_level = LVL,
  replicate = run,
  probs = c(0.05, 0.95),
  statistic = "median"
)

plot_forest(data = sum_data, 
            # shaded_interval = c(0.524,1.9),
            shaded_interval = c(shaded_interval[1],shaded_interval[2]),
            caption =
              paste0("Lower line represents the median of the summary statistics (Median, 5th and 95th quantiles).
              Upper lines represent the 90% CI of the individual statistics.
              Shaded interval corresponds to ",round(shaded_interval[1],2),", ",round(shaded_interval[2],2)),
            text_size = 3.5,
            x_lab = expression(paste("Normalized ",AUC[SS])),
            CI_label = "Median [90% CI]",
            plot_width = 8, # out of 12
            annotate_CI=T,
            nrow = 1)

mrggsave_last(stem = "pk-sim-renal-normalized-auc-norm-range", height = 6, width = 10)

data_auc_raw <- simresIIV2 %>% 
  select(-c(N_AUCSS, N_CMAXSS, N_CMINSS)) %>% 
  pivot_longer(AUCSS:CMAXSS) %>%
  subset(name=="AUCSS")

shaded_interval <- quantile(data_auc_raw$value[data_auc_raw$GRP=="Observed"], c(0.05,0.95))

sum_data2 <- summarize_data(
  data = data_auc_raw %>% dplyr::filter(GRP != "Observed"), 
  value = value,
  group = GRP,
  #group_level = LVL,
  replicate = run,
  probs = c(0.05, 0.95),
  statistic = "median"
)

plot_forest(data = sum_data2, 
            shaded_interval = c(shaded_interval[1],shaded_interval[2]),
            text_size = 3.5,
            caption =
              paste0("Lower line represents the median of the summary statistics (Median, 5th and 95th quantiles).
              Upper lines represent the 90% CI of the individual statistics.
              Shaded interval corresponds to ",round(shaded_interval[1],2),", ",round(shaded_interval[2],2)),
            x_lab = expression(paste(AUC[SS])),
            CI_label = "Median [90% CI]",
            plot_width = 8, # out of 12
            annotate_CI=T,
            nrow = 1)

mrggsave_last(stem = "pk-sim-renal-normalized-auc-raw-range", height = 6, width = 10)
