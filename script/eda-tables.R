#' --- 
#' title: Exploratory data analysis (EDA) tables
#' output: tex files and pdf preview
#' ---
#' 
#' # Scope
#' 
#' This document provides examples of the typical EDA
#' tables you might include in your report. This is not 
#' meant to be an exhaustive list and will not be appropriate
#' for all projects. 
#' It includes examples of using pmtables and how you can 
#' leverage information in your dataset yspec to name columns etc.

### Libraries ----------------------------
suppressPackageStartupMessages(library(tidyverse))
library(pmtables)
library(yspec)
library(texPreview)
library(here)
library(magrittr)
library(data.table)

### Directories ----------------------------
scriptDir <- here("script")
tabDir <- here("deliv", "table", "report")
dataDir <- here("data", "derived")

if(!file.exists(tabDir)) dir.create(tabDir)

thisScript <- "eda-tables.R"

set.seed(5238974)

# Set any script options ----------------------------
options(mrg.script = thisScript, pmtables.dir = tabDir)

# Read in dataset ----------------------------
dat = fread(file = here(dataDir, "analysis3.csv"), na.strings='.') 

# yspec details ----------------------------
specLo = here("data", "spec", "analysis3.yml")
spec <- ys_load(specLo)

# look at which namespaces are available in the yspec
ys_namespace(spec)

# include a tex alternative spec 
specTex <- ys_namespace(spec, "tex")

# define units for later (using the tex version of the spec)
units = ys_get_unit(specTex, parens = TRUE)

# define covariate names for later (using the tex version of the spec)
covlab = ys_get_short_unit(specTex, parens = TRUE, title_case=TRUE)


# open empty list for all tables that will eventually go in pdf preview
tableList <- list()


##### Created with new pmtables package ----------------------------

### For PK summary tables --------------------------------------
pkSum = dat %>% 
  yspec_add_factors(spec, STUDY, CP, RF, DOSE, SEQ) %>% 
  filter(is.na(C), SEQ==1) 


## Number and % of subjects, observations and BLQ per study
tableList$'pk-data-sum' = pkSum %>%
  pt_data_inventory(by = c("Study" = "STUDY_f")) %>%  
  stable(output_file = "pk-data-sum.tex") 

# use this to check how your table will look in the report template
# tableList$'pk-data-sum' %>% st2report() 

# Save the table out
tableList$'pk-data-sum' %>% stable_save()



## Number and % of subjects, observations and BLQ per study, panels: dose group
tableList$'pk-data-sum-per-dose' = pkSum %>%
  pt_data_inventory(stacked = TRUE,
                    by = c('Dose group' = "DOSE_f"),
                    panel = as.panel("STUDY_f", prefix="Study:")) %>% 
  stable(output_file = "pk-data-sum-per-dose.tex") %T>%
  # the T pipe (magrittr) can be used to view and then save the table in a single code chunk
  #st2report() %T>%
  stable_save()



# Subjects by study, renal function and dose group --------------------------------------

tableList$'rf-per-dose' = pkSum %>% 
  distinct(ID, DOSE_f, STUDY_f, RF_f, .keep_all = TRUE) %>% 
  pt_cat_wide(
    cols = vars("Renal function" = "RF_f"), 
    panel = as.panel("STUDY_f", prefix = "Study:"),
    by = c("Dose Group" = "DOSE_f")
  ) %>% 
  stable(output_file =  "rf-per-dose.tex") %T>% 
  # st2report() %T>%
  stable_save()


# Subjects by study, hepatic function and dose group --------------------------------------

tableList$'hepatic-per-dose' = pkSum %>% 
  distinct(ID, DOSE_f, STUDY_f, CP_f, .keep_all = TRUE) %>% 
  pt_cat_wide(
    cols = vars("Child Pugh Score" = "CP_f"), 
    panel = as.panel("STUDY_f", prefix = "Study:"), 
    by = c("Dose Group" = "DOSE_f")
  ) %>% 
  stable(output_file =  "hepatic-per-dose.tex") %T>% 
  # st2report() %T>%
  stable_save()




#  -------------------------------------- --------------------------------------
# Continuous summaries
# -------------------------------------- --------------------------------------

# Continuous time-independent covariates ------------------------------

covID = dat %>% 
  yspec_add_factors(spec, STUDY, CP, RF, SEQ) %>% 
  yspec_add_factors(spec, DOSE, .suffix = "") %>% 
  filter(is.na(C)) %>% 
  select(ID:TIME, AGE:CP, PHASE:SEQ_f)

# for time-independent covariates
timeIndCoDF = covID %>% 
  distinct(ID, .keep_all = TRUE)

##' Use yspec to make the abbreviations for the footer -----
##' This section uses the `ys_get_short_unit` function and 
##' the `flags` argument in the spec file (see yaml for coding)
##' to identify all the continuous covariates of interest and 
##' then use the information in the spec to make a footer 
##' for the table that explains all the abbreviations. 

labs <- ys_get_short_unit(specTex, parens = TRUE)   
contCovDF <- ys_filter(specTex, covariate)
contCovNames <- names(contCovDF)
abb = imap_chr(contCovNames, ~paste0(contCovNames[.y],": ", labs[[.x]]))
abb2 = toString(abb, sep=", ")   # to turn this into a single line footer

summaryNotes = c("n: number of records summarized,
                 SD: standard deviation,
                 Min: minimum, Max: maximum")

# summaries by study -------------------
tableList$'cont-covar-sum' =  timeIndCoDF %>%
  pt_cont_long(cols = contCovNames,
               panel = as.panel("STUDY_f", prefix = "Study:"),
               table = covlab # renaming covariate values in rows
  ) %>% 
  stable(output_file =  "cont-covar-sum.tex",
         # kept for reference if you prefer to decode the abbreviations in the footer
         # notes = paste(summaryNotes, abb2, sep = ", "), 
         notes = summaryNotes,
         note_config = noteconf(type = "minipage", width = 1)) %T>% 
  #st2report() %T>%
  stable_save()


# summaries by renal function -------------------
tableList$'cont-covar-sum-rf' = timeIndCoDF %>% 
  pt_cont_long(cols = contCovNames,
               panel = as.panel("RF_f", prefix = "Renal Function:"),
               table = covlab
  )  %>% 
  stable(output_file = "cont-covar-sum-rf.tex",
         notes = summaryNotes,
         note_config = noteconf(type = "minipage", width = 1))  %T>% 
  # st2report() %T>%
  stable_save()

# summaries by hepatic function -------------------
tableList$'cont-covar-sum-hepatic' = timeIndCoDF %>% 
  pt_cont_long(cols = contCovNames,
               panel = as.panel("CP_f", prefix = "Child pugh:"),
               table = covlab)  %>% 
  stable(output_file =  "cont-covar-sum-hepatic.tex",
         note_config = noteconf(type = "minipage", width = 1),
         notes = summaryNotes
  )  %T>% 
  # st2report() %T>%
  stable_save()

## save out all tables to the report template to
## check how they'll look with report margins etc.
st2report(
  tableList,
  ntex = 2,
  stem = "preview-eda",
  ## name of pdf preview
  output_dir = tabDir,
  show_pdf = interactive()
)
