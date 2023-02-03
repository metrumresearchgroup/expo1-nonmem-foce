options(repos = c(MPN = "https://mpn.metworx.com/snapshots/stable/2023-01-25//", CRAN = "https://cran.rstudio.com"))
message("repos set to: \n\t", paste0(getOption('repos'), collapse = "\n\t"))
source("renv/activate.R")
message("library paths set to: \n\t", paste0(.libPaths(), collapse = "\n\t"))

options('bbr.bbi_exe_path' = file.path(getwd(), "bin", "bbi"))

# render all mrggsave call as png as well for hashing/testing purposes
options("mrggsave.dev" = "pdf,png")
