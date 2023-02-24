# expo1-nonmem-foce

This repository contains examples of the various tasks associated with the
completion of a modeling project. The goal is to represent the most up to date
packages and methodologies for completing these tasks. It is expected that these
will evolve with time and that it would be best to check back periodically.

# Data

- PK model estimation data set 
  - location : `data/derived/analysis3.csv`
  - spec file : `/data/spec/analysis3.yml`
  - assembly : `da-pk-01.R`
  - exploratory data analysis : `eda-tables.R` and `eda-figures.R`
  - bootstrap parameter estimates : `data/boot/boot-106.csv`


# Scripts
- Data assembly: `da-pk-01.R`
- Model creation, submission & management: `model-management.R`
  - Examples code that maybe used in model-management.R: `model-management-demo.Rmd`
- Diagnostic plots are created in: `model-diagnostics.R`
  - this R script uses an Rmd template `diagnostic-templates > diagnostics-report.Rmd` 
  to make an html that will be saved to your model directory
- Model summary :`model-summary.Rmd`
- Creation and processing of boostrap runs: 
  - Generation and submission of bootstrap models: `boot-generate.Rmd`
  - Quick export of bootstrap parameter estimates: `boot-collect.R`
  - WIP Post processing of bootstrap runs: `bootstrapPostProcessing.Rmd`
- Parameter tables : 
  - Parameter key for all tables is now included in a yaml - `pk-param-key.yaml`
  - Parameter table for base model: `pk-base-model-table.R`
  - Parameter table for final model (incl. covariates)`pk-final-model-table.R`
  - Parameter table for bootstrap model (incl. covariates)`pk-final-model-table-boot.R`
  - Parameter table created without bbr i.e., with nonr (presented at JET) `pk-final-model-table-no-bbr.R`
  - Parameter table created without bbr using the pk-param-key.yaml `pk-final-model-table-no-yml.R`

- Study summary table: `study-summary.R`
  - Study description in `study-summary.yaml`
- Figures for report 
  - Goodness-of-fit evaluations from `model-diagnostics.R` with `diagnostics-report.Rmd`
  - Covariate effects evaluation from `forest-plots.R`
- Predictive checks:
  - VPC: `pk-vpc-final.R`
  - Numerical: `pk-pcheck-final.R`
- Simulation:
  - Exposure using EBE: `pk-ebe-simulate.R`


# Helper functions
- Helper functions for model management: `functions-model.R`
- Helper functions for scripts creating tables: `functions-tables.R`
- Helper functions for creating the study summary table : `functions-diagnostics.R`
- Helper functions for creating diagnostic plots : `functions-study-summary.R`

# Metworx Version
- metworx-21.08

# R Version
- 4.1.1



Copied from internal repo at 2467b4a8b9f0189aa84e06ff152ce8ed3e63d546

