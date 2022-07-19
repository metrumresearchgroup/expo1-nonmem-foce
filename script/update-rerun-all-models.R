# re-runs models 100-107 on the grid
source(here::here("tests", "helpers-for-tests.R"))

models <- purrr::map(
  here::here("model", "pk", 100:106),
  bbr::read_model
)

bbr::submit_models(
  models, 
  .bbi_args=list(overwrite=TRUE, threads = 1)
)

wait_for_qstat(.l = 600) # wait until models are done.