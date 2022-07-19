#' --- 
#' title: Study summaries for reporting
#' output: tex files
#' ---
#' 
#' # Scope
#' 
#' This document illustrates how you can use a yaml file containing 
#' a summary of your project studies to create the summary 
#' text and table needed for section 3. Data sources and description
#' 

### Libraries ----------------------------
library(tidyverse)
library(pmtables)
library(yaml)
library(here)
library(glue)
library(magrittr)

### Directories ----------------------------
scriptDir = here("script")
tabDir = here("deliv", "table", "report")

if(!file.exists(tabDir)) dir.create(tabDir)

thisScript = "study-summary.R"

set.seed(5238974)

# Grab some helper functions ----------------------------
source(file.path(scriptDir, 'functions-study-summary.R'))

# Set options for saving out table ----------------------------
options(mrg.script = thisScript, pmtables.dir = tabDir)


# Read in yaml description of studies ----------------------------
# and turn into a dataframe
sdf = yaml_as_df(file.path(scriptDir, "study-summary.yaml"))

## Create summary text for report "Data Sources and Description" -------------
sdf %>% 
  select(fileEx, Study, Title, Short, Conc) %>% 
  rename(Design = Short, 
         Conclusion = Conc) %>% 
  pwalk(perStudy, out_dir = tabDir)



# Create summary table for the report ----------------------------
footAbbrev = "Abbreviations: BID = twice daily; 
                              DG = Dose Group; 
                              Obs = Observations"

align = c(
  Study = col_ragged(2.5),
  Short = col_ragged(3),
  Phase = 'c',
  nSub = col_fixed(2.5, center=TRUE),
  nDose = col_ragged(2),
  D_levels = col_ragged(2),
  ob_n_time = col_ragged(6)
  )


df = sdf %>% 
  mutate(ob_n_time = paste0(obTime, " (N = ", nTime, ")")) %>% 
  select(Study, Short, Phase, nSub, nDose, D_levels, ob_n_time) %>% 
  st_new() %>%
  st_rename("Design" = "Short",
            "Number of ... subjects" = "nSub", 
            "Dose ...regimen" = "nDose", 
            "Dose levels" = "D_levels", 
            "Planned observations" = "ob_n_time") %>% 
  st_align(align) %>% 
  st_hline(from = "Study") %>% 
  st_notes(footAbbrev) %>% 
  st_noteconf(type="minipage", width = 1) %>% 
  st_files(output = "study-summary-table.tex") %>% 
  st_sizes(font="small", row = 2, header_row = -1) %>% 
  st_bold("Study", pattern = "DEMO") %>% 
  stable(cols_bold = TRUE) %>%
  as_lscape() %T>% 
  # st2report(ntex=2) %T>%
  stable_save()

if(interactive()) st2report(df, ntex = 2) 

### Example only - if the study summary runs over multiple pages
# 
# sdf_long = map_dfr(1:3, function(i) {
#   mutate(sdf, Study = paste0(Study, "-",i))
# })
# 
# 
# ## With code that uses pipeable functions
# df_long_pipe = sdf_long %>%
#   mutate(ob_n_time = paste0(obTime, " (N = ", nTime, ")")) %>%
#   select(Study, Short, Phase, nSub, nDose, D_levels, ob_n_time) %>%
#   st_new() %>%
#   st_rename("Design" = "Short",
#             "Number of ... subjects" = "nSub", 
#             "Dose ...regimen" = "nDose", 
#             "Dose levels" = "D_levels", 
#             "Planned observations" = "ob_n_time") %>% 
#   st_align(align) %>% 
#   st_hline(from = "Study") %>% 
#   st_notes(footAbbrev) %>% 
#   st_noteconf(type="minipage", width = 1, table_skip = -0.45) %>% 
#   st_files(output = "study-summary-table.tex") %>% 
#   st_sizes(font="small", row = 2, header_row = -1.5) %>% 
#   st_bold("Study", pattern = "DEMO") %>% 
#   stable_long(cols_bold = TRUE,
#               # Caption and label for long table should be include in R or
#               # you can define a macro in the table (using lt_cap_macro) and then
#               # replace the macro with a caption and label directly in the
#               # latex document.
#               lt_cap_text = "Clinical study summary",
#               lt_cap_label = "trialSum") %>% 
#   as_lscape() %T>% 
#   # st2report(ntex=2) %T>% 
#   stable_save()
# 
# 
# 
# ## Remember tell latex the table is landscape e.g.
# ## and landscape table currently need labels and captions included in the R code or
# ## they won't show up well in the report
# # \begin{landscape}
# # \input{../table/report/tablongStudySum.tex}
# # \end{landscape}