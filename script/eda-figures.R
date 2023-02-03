#' --- 
#' title: Exploratory data analysis (EDA) figures
#' output: png and pdf preview
#' ---
#' 
#' # Scope
#' 
#' This document provides examples of the typical EDA
#' figures you might include in your report. This is not 
#' meant to be an exhaustive list and will not be appropriate
#' for all projects. 
#' It includes examples of using pmplots and how you can 
#' leverage information in your dataset yspec to name panels/axis etc.


### Libraries ----------------------------
library(tidyverse)
library(yaml)
library(yspec)
library(pmplots)
library(mrggsave)
library(mrgmisc)
library(here)
library(data.table)


### Directories ----------------------------
scriptDir = here("script")
figDir = here("deliv", "figure", "report")
dataDir = here("data", "derived")

if(!file.exists(figDir)) dir.create(figDir)

thisScript = "eda-figures.R"

set.seed(5238974)

# Helper functions ----------------------------
asNum  = function(f){ return(as.numeric(as.character(f))) }


# Set any script options ----------------------------
options(mrg.script = thisScript, mrggsave.dir = figDir)


# Read in dataset ----------------------------
dat = fread(file = here(dataDir, "analysis3.csv"), na.strings = '.')


# yspec details ----------------------------
# Aim is to use the data in the yspec file to label the columns automatically
# This uses the 'short' name and assumes any units are defined
# Alternatively, setting .unitOnly=T will combine the column abbreviation 
# with the yspec units

# Tell R where to find the yml
specLo = here("data", "spec", "analysis3.yml")

# load in the spec file
spec = ys_load(specLo)

# look at which namespaces are available in the yspec
ys_namespace(spec)

# include a tex alternative spec 
specTex = ys_namespace(spec, "tex")

# Use spec to grab column labels for plots'
contCovDF = ys_filter(spec, covariate)
contCovNames = names(contCovDF) 
contCovarListTex = axis_col_labs(specTex, 
                                 contCovNames, 
                                 title_case = TRUE, 
                                 short = 10) 

# if you wanted to manually change facet labels, in this case, 
# put units on a new line
contCovarListTex["EGFR"] = "EGFR//EGFR \n(ml/min/1.73m^2)"


# Refactor table for plots ----------------------------
dat2 = dat %>% 
  yspec_add_factors(spec, STUDY, CP, RF, DOSE) %>% 
  mutate(DoseNorm = DV/DOSE)


# Make a table of unique covariates ----------------------------
covar = dat2 %>% 
  distinct(ID, .keep_all = TRUE) %>% 
  rename(Study = STUDY_f) %>% 
  select(ID, AMT, AGE:ALB, LDOS, STUDY, RF, CP, 
         Study:DoseNorm, NUM) 


# Continuous covariate correlation plot ----------------------------

ContVCont = covar %>%
  eta_pairs(etas = contCovarListTex) +
  rot_x(50) 

ContVCont

mrggsave(ContVCont,
         stem = "cont-vs-cont",
         width = 6, height = 7)


# Boxplots of Continuous vs categorical ----------------------------

##### Renal Function Group ##########

## Continuous vs categorical by total population
renal_contCat_Total = covar %>% 
  wrap_cont_cat(x = "RF_f//Renal Function Group",
                y = contCovarListTex,
                use_labels = TRUE
  ) + rot_x(35)

# renal_contCat_Total

## Continuous vs categorical by study
renal_contCat_Study = map(contCovarListTex, function(.y){
  cont_cat(df = covar, x = 'RF_f//Renal Function Group', y = .y) + 
    facet_wrap(~STUDY, labeller = label_both) +
    rot_x(35)
})


##### Hepatic Function Group ##########

## Continuous vs categorical by total population
hep_contCat_Total = covar %>% 
  wrap_cont_cat(x = 'CP_f//Hepatic Function Group',
                y = contCovarListTex,
                use_labels = TRUE
  ) + rot_x(35)

# hep_contCat_Total

## Continuous vs categorical by study
hep_contCat_Study = map(contCovarListTex, function(.y){
  cont_cat(df = covar, x = 'CP_f//Hepatic Function Group', y = .y) + 
    rot_x(45) + 
    facet_wrap(~Study, labeller = label_both)
})

CatvContList = list(renal_contCat_Total, renal_contCat_Study,
                    hep_contCat_Total, hep_contCat_Study)

mrggsave(CatvContList, stem = "cat-vs-cont")



# Concentration-time plots ----------------------------
dat3 = dat2 %>% filter(EVID==0, BLQ==0)

xLabT = 'Time (h)'
xLabTAD = 'Time after dose (h)'
yLab = 'Concentration (ng/mL)'
yLabNorm = 'Dose-normalized concentration (ng/mL*mg)'


## Concentration-time plot helper functions -------------------------------
.plot_color_theme = function(...) {
  scale_color_brewer(..., palette = "Set1")
}

##' df = dataframe for plotting
##' study = name of study to filter dataset on 
##' tlo = lower bound to filter TIME on (defaults to -inf)
##' thi = upper bound to filter TIME on (defaults to inf)
##' tadlo= lower bound to filter TAD on (defaults to -inf)
##' tadhi= upper bound to filter TAD on (defaults to inf)
filter_df = function(df, 
                     study = NULL, 
                     tlo = -Inf, 
                     thi = Inf,
                     tadlo = -Inf, 
                     tadhi = Inf){
  df = df %>% filter(TIME >= tlo & TIME <= thi)
  df = df %>% filter(TAD >= tadlo & TAD <= tadhi)
  if (!is.null(study)) df = df %>% filter(STUDY_f == study)
  return(df)
}

##' .df = dataframe for plotting
##' .logy = put y on a log scale T/F
##' .by_group = column name of grouping variable for facet wrap
##' .color = column name to use for colouring of data in plots
##' .group = column name to use for grouping data in plots
##' .legendName = name for legend items
##' .x = column to be used on x axis (default = TIME)
##' .y = column to be used on y axis (default = DV)
##' .xLab = label for x-axis
##' .yLab = label for y-axis
##' .incLine = to include line between groups
##' .smooth = to include smooth loess line
plot_Conc = function(.df, 
                     .logy = FALSE, 
                     .by_group = NULL,
                     .color = NULL, 
                     .group = NULL, 
                     .legendName = NULL,
                     .x = TIME, 
                     .y = DV, 
                     .xLab = xLabT, 
                     .yLab = yLab, 
                     .incLine = T, 
                     .smooth = F) {
  
  p = ggplot(data = .df, aes(x = {{.x}}, y = {{.y}})) +
    geom_point(aes(color = {{.color}})) +
    labs(x = .xLab, y = .yLab, colour = NULL)  + 
    .plot_color_theme(name = .legendName) +
    pm_theme()
  if(.incLine){
    p = p + geom_line(aes(color = {{.color}}, group = {{.group}}))
  }
  if(.smooth){
    p = p + geom_smooth(aes(color = {{.color}}, group = {{.group}}),
                        method = 'loess', formula = 'y ~ x')
  }
  if (!is.null(.by_group)) {
    p = p + facet_wrap( ~.data[[.by_group]], scales = "free", ncol = 2)
  }
  if (.logy) { 
    p = p + scale_y_log10()
  }
  p
}

# Study 1 ----------------
df01 =  dat3 %>% filter_df(thi = 24, study = '101-DEMO-001')  # first dose

#Semi-log first dose
MET1 = plot_Conc(.df=df01, .color=DOSE_f, .group=ID, 
                 .logy=TRUE, .legendName="Dose Group")

#Semi-log first dose (dose-normalized)
MET1a = plot_Conc(.df=df01, .y=DoseNorm, .yLab = yLabNorm, .logy=TRUE, 
                  .legendName="Dose Group", .color=DOSE_f, .group=ID)

#Semi-log first dose (smooth by dose)
MET1b = plot_Conc(.df=df01, .color=DOSE_f, .group=DOSE_f, .logy=TRUE, 
                  .legendName="Dose Group", .incLine = F, .smooth = TRUE)

## Example for plotting tad - not necessarily needed in the report
MET1_tad = plot_Conc(.df=df01, .color =DOSE_f, .group =ID, .logy=TRUE, 
                     .legendName="Dose Group", .x=TAD, .xLab = xLabTAD)


# Study 2 ----------------
df02 =  dat3 %>% filter_df(thi = 13, study = '101-DEMO-002')  # first dose
df02b = dat3 %>% filter_df(study = '101-DEMO-002')            # full time course
df02c = dat3 %>% filter_df(tlo = 126, study = '101-DEMO-002') # last dose

#Semi-log first dose
MET2 = plot_Conc(.df=df02, .color=DOSE_f, .group=ID, .logy=TRUE, 
                 .legendName="Dose Group")

#Semi-log first dose (dose-normalized)
MET2a = plot_Conc(.df=df02, .y=DoseNorm, .yLab = yLabNorm, .logy=TRUE, 
                  .legendName="Dose Group", .color=DOSE_f, .group=ID)

#Semi-log first dose (smooth by dose)
MET2b = plot_Conc(.df=df02, .color=DOSE_f, .group=DOSE_f, .logy=TRUE, 
                  .legendName="Dose Group", .incLine = F, .smooth = T)

#Semi-log total time course
MET2c =plot_Conc(.df=df02b, .color=DOSE_f, .group=ID, .logy=TRUE, 
                  .legendName="Dose Group")

#Semi-log last dose
MET2d = plot_Conc(.df=df02c, .color=DOSE_f, .group=ID, .logy=TRUE, 
                   .legendName="Dose Group")

#Semi-log last dose (dose-normalized)
MET2e = plot_Conc(.df=df02c, .y=DoseNorm, .yLab = yLabNorm, .logy=TRUE, 
                   .legendName="Dose Group", .color=DOSE_f, .group=ID)


# Study 3 ----------------
df03 =  dat3 %>% filter_df(thi = 13, study = '201-DEMO-003')  # first dose
df03b =  dat3 %>% filter_df(study = '201-DEMO-003')           # full time course
df03c =  dat3 %>% filter_df(tlo = 130, study = '201-DEMO-003')# last dose

#Semi-log first dose
MET3 = plot_Conc(.df=df03, .color =RF_f, .group =ID, .logy=TRUE, 
                 .legendName="Renal function")

#Semi-log first dose (dose-normalized)
MET3a = plot_Conc(.df=df03, .y=DoseNorm, .yLab = yLabNorm, .logy=TRUE,
                  .legendName="Renal function", .color=RF_f, .group=ID)

#Semi-log first dose (smooth by dose)
MET3b = plot_Conc(.df=df03, .color=RF_f, .group=RF_f, .logy=TRUE, 
                  .legendName="Renal function", .incLine = FALSE, .smooth = TRUE)

#Semi-log first dose
MET3c = plot_Conc(.df=df03b, .color=RF_f, .group=ID, .logy=TRUE, 
                  .legendName="Renal function")

#Semi-log first dose (smooth by dose)
MET3d = plot_Conc(.df=df03b, .color=RF_f, .group=RF_f, .logy=TRUE, 
                  .legendName="Renal function", .incLine = FALSE, .smooth = TRUE)

#Semi-log last dose (smooth by dose)
MET3e = plot_Conc(.df=df03c, .color=RF_f, .group=RF_f, .logy=TRUE, 
                  .legendName="Renal function", .incLine = FALSE, .smooth = TRUE)


# Study 4 ----------------
df04  =  dat3 %>% filter_df(thi = 24, study = '201-DEMO-004')  # first dose

#Semi-log first dose
MET4 = plot_Conc(.df=df04, .color =CP_f, .group =ID, .logy=TRUE, 
                 .legendName="Child Pugh Score")

#Semi-log first dose (dose-normalized)
MET4a = plot_Conc(.df=df04, .y=DoseNorm, .yLab = yLabNorm, .logy=TRUE,
                  .legendName="Child Pugh Score", .color=CP_f, .group=ID)

#Semi-log first dose (smooth by dose)
MET4b = plot_Conc(.df=df04, .color=CP_f, .group=CP_f, .logy=TRUE, 
                  .legendName="Child Pugh Score", .incLine = FALSE, .smooth=TRUE)

plotlist = list(MET1,MET1a,MET1b,
                 MET2,MET2a,MET2b,MET2c,MET2d,MET2e,
                 MET3,MET3a,MET3b,MET3c,MET3d,MET3e,
                 MET4,MET4a,MET4b)

mrggsave(plotlist,
         stem = "pk-plots", width = 6)

# note this saves each plot in the list to a new file
# mrggsave(plotlist, 
#          tag = "pk-plots",
#          onefile=FALSE,
#          width=5, height=5)



## Concentration-time plots per ID -------------------------------
# These plots are not necessarily needed for every analysis but an example 
# included for completeness

split_Obs = dat2 %>% filter(EVID==0) %>% 
  mutate(ID = asNum(ID)) %>% 
  mutate(PLOTS = ids_per_plot(ID, 4)) %>% # default is 9 per subplot
  split(.[["PLOTS"]])

split_Dose = dat2 %>% filter(EVID==1) %>% 
  mutate(ID = asNum(ID)) %>% 
  mutate(PLOTS = ids_per_plot(ID, 4)) %>% # default is 9 per subplot
  split(.[["PLOTS"]])

# This function can/should be edited to address project specific questions
p_conc_time = function(dfO, dfD) {
  ggplot() + 
    geom_point(data=dfO, aes(x=TIME, y=DV)) +
    geom_line(data=dfO, aes(x=TIME, y=DV, group=ID)) +
    # dashed vertical line to indicate dose times
    geom_vline(data=dfD, aes(xintercept=TIME), 
               linetype="dashed", colour="grey") +
    facet_wrap(~STUDY+ID, scales= "free") +
    pm_theme() + theme(legend.position = "none") + 
    labs(x = xLabT, y = yLab)
}

IDplotList = map2(split_Obs, split_Dose, p_conc_time) 

mrggsave(IDplotList, stem = "id-dose-dv")
