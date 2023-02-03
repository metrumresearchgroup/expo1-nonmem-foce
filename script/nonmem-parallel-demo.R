library(bbr)
library(here)
options("bbr.bbi_exe_path" = here("bin/bbi")) # use bbi inside project instead of default system version
.v <- bbi_version()
if (.v == "" || package_version(.v) < 3.2) use_bbi() # this will install the latest version

MODEL_DIR <- here("model", "pk-parallel")

# models 100-106 are too fast/simple to effectively run in parallel,
# model 200 is intentionally more complex for demonstrating parallel execution
mod106 <- read_model(here("model", "pk", 106))
mod200 <- read_model(file.path(MODEL_DIR, 200))
model_diff(mod106, mod200)

# submits to run on SGE grid by default 
# pass `threads` through `.bbi_args` to run in parallel
submit_model(mod200, .bbi_args = list(threads = 8))

# check the queue for the SGE grid
# job will be "pending" until compute nodes come up
system("qstat -f")

# wait to finish and then look at summary
wait_for_nonmem(mod200)
model_summary(mod200)

######################
# testing threads 
######################

# Parallelizing does not scale linearly, and the ideal number of threads
# varies from model to model. bbr::test_threads() helps you estimate
# the ideal number of threads for a given model, empirically.

test_mods <- test_threads(mod200, c(4, 8, 16, 32), .cap_iterations = 100)
res <- check_run_times(test_mods) # this will hold your console until all test models finish
View(res)

# clean up test models
delete_models(test_mods)

####################################################
# ...or load in previously run results to demonstrate
# Note: this test was run on 4-vCPU compute nodes
#res <- read.csv(here("data", "derived", "test-threads-200.csv"))
####################################################


##############################
# plot to visualize run times
##############################

library(ggplot2)
ggplot(res, aes(x = threads, y = estimation_time)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = res$threads, minor_breaks = NULL)
