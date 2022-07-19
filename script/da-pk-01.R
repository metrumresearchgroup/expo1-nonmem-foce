#' ---
#' title: Assemble population-PK model ready data sets
#' author: Kyle Baron
#' date: "`r date()`"
#' output: html_document
#' ---

library(tidyverse)
library(yspec)
library(here)
library(data.table)
library(haven)
setwd(here("script"))

#' 
#' # Scope
#' 
#' - first, create `analysis2`
#' - then create `analysis3`
#' - the source for both is `analysis1`
#' 

#' 
#' # Read in source data
#' 
#' The base data set for this is `analysis1.csv`
#' We are creating `analysis2`; aiming for short data set name 
#' 
base <- read_csv("../data/derived/analysis1.csv", na = '.')

#' Load the spec object
spec <- ys_load("../data/spec/analysis2.yml")

#' 
#' - Add STUDYN as the study number
#' - Code STUDY as a character study identifier
#' - Add USUBJID - unique subject ID
#' 
data <- mutate(
  base,
  STUDYN = STUDY,
  STUDY = case_when(
    STUDY==1 ~ "101-DEMO-001", 
    STUDY==2 ~ "101-DEMO-002", 
    STUDY==3 ~ "201-DEMO-003", 
    STUDY==4 ~ "201-DEMO-004"
  ),
  USUBJID = paste0(STUDY, formatC(ID, width = 4, flag = '0'))
)

#' - Add numeric `DOSE` identifier
#' - Simulate `SEX` indicator

id <- 
  data %>%
  filter(EVID==1) %>% 
  rename(DOSE = AMT) %>% 
  distinct(USUBJID, DOSE, STUDYN) 

set.seed(12345)
id <- mutate(id, SEX = rbinom(n(), 1, 0.42)) 

#' 
#' Add `ACTARM` based on dose and regimen
#' Studies 2 and 3 are multiple dose x7
#' 
sad <- 
  id %>%
  filter(STUDYN==1) %>% 
  mutate(ACTARM = paste0("DEMO ", DOSE, " mg"))
mad <- 
  id %>%
  filter(STUDYN==2) %>%  
  mutate(ACTARM = paste0("DEMO ", DOSE, " mg qd x7"))
renal <- 
  id %>%
  filter(STUDYN==3) %>%  
  mutate(ACTARM = paste0("DEMO ", DOSE, " mg qd x7"))
hepatic <- 
  id %>% 
  filter(STUDYN==4) %>%  
  mutate(ACTARM = paste0("DEMO ", DOSE, " mg"))

id_by_study <- bind_rows(sad, mad, renal, hepatic)

data <- left_join(data, id_by_study) # STUDYN and USUBJID

#' Number the rows
data <- mutate(data, NUM = row_number())

#' Make sure columns are in the right order and everything is there
data <- select(data, names(spec))

#' Check the data against the spec
ys_check(data, spec)

#' Write the define document to `../data/derived`
ys_document(
  spec, 
  type = "regulatory", 
  output_dir = "../data/derived", 
  output_file = "analysis2.pdf", 
  build_dir = definetemplate(), 
  author = "Kyle Baron"
)

#' Write the data
fwrite(
  data, 
  file = "../data/derived/analysis2.csv", 
  na = '.', 
  quote = FALSE
)

#' Label the data
data <- ys_add_labels(data, spec)

#' Write sas xport file - version 5
write_xpt(
  data, 
  path="../data/derived/analysis2.xpt", 
  version = 5, 
  name = "data2"
)

#' 
#' # Start analysis3 data set
#' 

spec <- ys_load("../data/spec/analysis3.yml")
data <- read_csv("../data/derived/analysis2.csv", na = '.')

#' Rename `CLCR` to `EGFR`
spec$EGFR

data <- rename(data, EGFR = CRCL)

data <- mutate(data, NUM = row_number())

data <- select(data, names(spec))

ys_check(data,spec)

#' Write the data
fwrite(
  data, 
  file = "../data/derived/analysis3.csv", 
  na = '.', 
  quote = FALSE
)

#' Label the data
data <- ys_add_labels(data, spec)

#' Write sas xport file - version 5
write_xpt(
  data, 
  path = "../data/derived/analysis3.xpt", 
  version = 5, 
  name = "data3"
)

ys_document(
  spec,
  type = "regulatory", 
  output_dir = "../data/derived", 
  output_file = "define.pdf", 
  build_dir = definetemplate(), 
  author = "MetrumRG"
)
