### Purpose -------------------------------
##' Produce diagnostic plots using an Rmd template 
##' (located in the diagnostics-template folder)
##' By defining model specific characteristics here, we can simply render the 
##' Rmd template to produce the diagnostics relevant to a specific model
##' 
##' This particular script focuses on producing diagnostics from a single template
##' but it may be necessary for you to have multiple templates (depending how 
##' different the models are).
##' This script is simply meant to provide an example of how you can run the 
##' same diagnostic template for multiple similar models. In this case, the template
##' focuses on creating diagnostics that will be included in the final report 
##' but please note that these are just examples for this 
##' report. The template and choice of diagnostics should be modified to reflect 
##' the project specific questions being addressed in your project. 
##' 
##' Support for parameterized reports can be found 
##' https://bookdown.org/yihui/rmarkdown/parameterized-reports.html


### Libraries ----------------------------
library(tidyverse)
library(here)
library(rmarkdown)

### source helper functions ----------------------------
source(here("script/functions-diagnostics.R"))

### Model directory -------------------
modelDir <- here("model", "pk")


### diagnostic-templates > diagnostics-report.Rmd ----------------------------
##' The final results included in the report were created with the 
##' diagnostics-report.rmd. This script was written with the report
##' specifications in mind and generates the pdf figures appropriate
##' to this report. 
##' Please go to the diagnostics-report.Rmd to see the fields that can be edited 
##' directly in this R script. 

rmd_template <- here("script", "diagnostic-templates", "diagnostics-report.Rmd")

### The idea here is that you will build up the model diagnostic code shown below 
##' as you work though you project. We recommend keeping this code in case you need
##' to re-run the diagnostic plots for some reason - it saves you re-typing all the 
##' model-specific details to re-create the diagnostics for multiple models. 


### First model - run 100 ----------------------------
##' This first example will be heavily commented so you can follow what is expected 
##' with each option but note that if you plan to the default setting you defined in
##' your template.Rmd then you don't need to redefine them here (see later examples below)

modelSpecifics <- list(
                      # Define name of spec (this example assumes it is located within data > spec)
                      yspec = "analysis3.yml",
                      # Vector of covariates to be plotted (as they appear in the spec)
                      contCov = c("AGE","WT","ALB","EGFR"), 
                      catCov = c("STUDY", "RF", "CP", "DOSE"),
                      # Vector of etas to be used by pmplots 
                      # if "ETA1", "ETA2" etc. are passed then this is how they 
                      # will appear in the plots. If "NONMEMName//DisplayName" is
                      # passed in then the plots will include the display name
                      etas = c("ETA1//ETA-KA", "ETA2//ETA-V/F", "ETA3//ETA-CL/F"),
                      # Set whether html file this code creates should include 
                      # the plot code and/or figures and whether you want to also
                      # save the figures as pdfs.
                      # These are actually defaults but included here as an example 
                      # of how simply they can be turned on and off
                      include_code = TRUE,
                      include_plots = TRUE,
                      run_mrggsave = TRUE
)

# The `model_diagnostics()` helper renders the Rmd template to an HTML file 
# in your model output directory and creates pdf/png versions of each figure in deliv. 
#
# You can pass a bbr model object, which is particularly useful
# if you're doing this somewhere like a model management or submission script
mod <- bbr::read_model(file.path(modelDir, 100))

mod %>%
  model_diagnostics(
    modelSpecifics,
    template = rmd_template
  )


### run 101 ----------------------------

modelSpecifics <- list(
              # note, if these are applicable to multiple models they can be defined once and reused
              contCov = c("AGE", "WT", "ALB", "EGFR"),
              catCov = c("STUDY", "RF", "CP", "DOSE"),
              etas = c("ETA1//ETA-KA", "ETA2//ETA-V/F", "ETA3//ETA-CL/F")
)

# To open the diagnostic html programmatically, pipe the `model_diagnostics()` into `browseURL()`
# (This works because `model_diagnostics()` invisibly returns the path to the rendered HTML file.)
mod <- bbr::read_model(file.path(modelDir, 101))

mod %>%
  model_diagnostics(
    modelSpecifics,
    template = rmd_template
  ) %>%
  browseURL()



### run 102 - Base model ----------------------------

modelSpecifics <- list(
                      contCov = c("AGE", "WT","ALB","EGFR"),
                      catCov = c("STUDY", "RF", "CP", "DOSE"),
                      etas = c("ETA1//ETA-KA", "ETA2//ETA-V/F", "ETA3//ETA-CL/F")
                  )

### You can also pass a path to a model output directory (instead of a bbr object)
model_diagnostics(
  file.path(modelDir, 102),
  modelSpecifics,
  template = rmd_template
) %>%
  browseURL()


### run 103 ----------------------------

# Example: params can be defined directly in `model_diagnostics()` call if you prefer
model_diagnostics(
  .mod = file.path(modelDir, 103),
  .p = list(
    contCov = c("AGE", "WT", "ALB", "EGFR"),
    catCov = c("STUDY", "RF", "CP", "DOSE"),
    etas = c("ETA1//ETA-KA", "ETA2//ETA-V/F", "ETA3//ETA-CL/F")
  ),
  template = rmd_template
) %>%
  browseURL()


### run 104 ----------------------------

modelSpecifics = list(
                      contCov = c("AGE", "WT", "ALB", "EGFR"),
                      catCov = c("STUDY", "RF", "CP", "DOSE"),
                      etas = c("ETA1//ETA-KA", "ETA2//ETA-V/F", "ETA3//ETA-CL/F")
)

model_diagnostics(
  file.path(modelDir, 104),
  modelSpecifics,
  template = rmd_template
) %>%
  browseURL()


### run 105 ----------------------------

## if nothing has changed compared to the previous `modelSpecifics` list, 
## you can simply pass through the previously defined list.

model_diagnostics(
  file.path(modelDir, 105),
  modelSpecifics,
  template = rmd_template
) %>%
  browseURL()


### run 106 - Final model ----------------------------

model_diagnostics(
  file.path(modelDir, 106),
  modelSpecifics,
  template = rmd_template
) %>%
  browseURL()


### run 106 - Final model Individual plots ----------------------------

## Use the `id-dv-pred-plots.Rmd` for generating plots for individuals.
##
## Passing `include_plots = FALSE` prevents the plots showing in the HTML doc (to speed up rendering),
## but still saves the plots out to individual files via `mrggsave`.
modelSpecifics = list(
                      yspec = "analysis3.yml",
                      drugNameUnits = "Mockdralazine concentration (mg/L)",
                      include_plots = FALSE 
)

model_diagnostics(
  file.path(modelDir, 106),
  modelSpecifics,
  template = here("script", "diagnostic-templates", "id-dv-pred-plots.Rmd")
)
