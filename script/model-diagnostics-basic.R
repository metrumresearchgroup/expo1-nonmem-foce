##' The purpose of this script is just to show some of the options available with 
##' the diagnostics-basic.Rmd. There is no reason you have to keep all of these 
##' calls somewhere. You can just add the simple example to your
##' model-management script and over-write model specific information


### Libraries ----------------------------
library(tidyverse)
library(rmarkdown)
library(here)
library(bbr)

### source helper functions ----------------------------
source(here("script/functions-diagnostics.R"))



#### Example 1 - Simple example ----------------------------

### define model specifics
### (useful if you're going to reuse the same `modelSpecifics` for multiple models)
modelSpecifics <- list(yspec = "data/spec/analysis3.yml",
                    drugNameUnits = "concentration (mg/L)"
)

### Pass path to model output dir to render diagnostics and pop them open in a new window
here("model", "pk", 102) %>%
  model_diagnostics(modelSpecifics) %>%
  browseURL()



#### Example 2 - testing overwriting covariates ----------------------------
## Not meant to be something we actually recommend in the example project analysis

### define model specifics 
modelSpecifics <- list(
  diagContCov = c("AGE"),
  diagCatCov = c("RF", "CP")
)

### Can also pass a bbr model object (anything with class `bbi_model`)
mod <- read_model(here("model", "pk", 102))

### Render diagnostics and pop them open in a new window
mod %>% 
  model_diagnostics(modelSpecifics) %>%
  browseURL()



#### Example 3 - testing dropping ETAs  ----------------------------
## Not meant to be something we actually recommend in the example project analysis
## but it maybe useful if you have fixed ETAs that don't need plots or multiple IOV etas

### Render diagnostics and pop them open in a new window
here("model", "pk", 106) %>% 
  model_diagnostics(list(excludeETAs = c("ETA1"))) %>%
  browseURL()
