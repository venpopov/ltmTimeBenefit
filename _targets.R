# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(crew)
library(purrr)
library(dplyr)
library(tidyr)

# Set target options:
tar_option_set(
  controller = crew_controller_local(workers = 10)
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# use quote because of https://github.com/ropensci/tarchetypes/discussions/105
sim1_hyperparameters <- expand_grid(
  exclude_sp1 = c(TRUE, FALSE),
  priors = c(
    quote(list()),
    quote(list(gain = list(mean = 25, sd = 0.1))),
    quote(list(rate = list(mean = 0.1, sd = 0.01)))
  ),
  data = list(quote(exp1_data_agg), quote(exp2_data_agg))
)

sim1_hyperparameters <- mutate(
  sim1_hyperparameters,
  priors_scenario = map_chr(priors, \(x) paste(names(x), collapse = "")),
  exp = stringr::str_extract(map_chr(data, as.character), "[0-9]")
)

sim2_hyperparameters <- expand_grid(
  exclude_sp1 = c(TRUE, FALSE),
  rate = seq(0.005, 0.2, by = 0.005),
  data = c(quote(exp1_data_agg), quote(exp2_data_agg))
)

sim2_hyperparameters <- mutate(
  sim2_hyperparameters,
  priors = c(quote(list(rate = list(mean = rate, sd = 0.0001)))),
  priors = lapply(1:n(), \(i) {
    priors[[i]]$rate$mean <- rate[i]
    priors[[i]]
  }),
  exp = stringr::str_extract(map_chr(data, as.character), "[0-9]")
)

sim3_hyperparameters <- sim1_hyperparameters |>
  mutate(data = syms(paste0(data, "_enc")))

# Pipeline
list(
  # Data preprocessing
  tar_target(data_files, c("data-raw/Exp1.Rdata", "data-raw/Exp2.Rdata"), format = "file", deployment = "main"),
  tar_target(exp1_data, get_data(data_files[1], longgap = 3000), deployment = "main"),
  tar_target(exp2_data, get_data(data_files[2], longgap = 6000), deployment = "main"),
  tar_target(exp1_data_agg, aggregate_data(exp1_data), deployment = "main"),
  tar_target(exp2_data_agg, aggregate_data(exp2_data), deployment = "main"),

  # Models
  ## fits1 simulates the model reported in the draft varying whether the first chunk is excluded
  ##   from the likelihood calculation, the experiment (1 vs 2), and three type of priors (none,
  ##   prior on gain, prior on rate)
  tar_map_rep(
    fits1,
    values = sim1_hyperparameters,
    command = estimate_model(
      start = start_fun(),
      data = data,
      exclude_sp1 = exclude_sp1,
      priors = priors,
      two_step = TRUE,
      simplify = TRUE
    ),
    batches = 10,
    reps = 10,
    names = tidyselect::any_of(c("priors_scenario", "exclude_sp1"))
  ),
  ## fits2 is similar to fits 1 but has a very narrow prior on a range of rates. Goal is to
  ##   check for parameter identifiability trade-offs
  tar_map_rep(
    fits2,
    values = sim2_hyperparameters,
    command = estimate_model(
      start = start_fun(),
      data = data,
      exclude_sp1 = exclude_sp1,
      priors = priors,
      two_step = TRUE,
      simplify = TRUE
    ),
    batches = 10,
    reps = 10,
    names = tidyselect::any_of(c("rate", "exclude_sp1"))
  ),
  ## fits3 is similar to fits 1, but we include the encodingtime of 0.9s in the recovery time.
  ##  This assumes that resources are depleted immediately upon presentation of the item (as in
  ##  Popov & Reder, 2020), and that recovery is always ongoing.
  tar_target(exp1_data_agg_enc, mutate(exp1_data_agg, ISI = ISI + 0.9), deployment = "main"),
  tar_target(exp2_data_agg_enc, mutate(exp2_data_agg, ISI = ISI + 0.9), deployment = "main"),
  tar_map_rep(
    fits3,
    values = sim3_hyperparameters,
    command = estimate_model(
      start = start_fun(),
      data = data,
      exclude_sp1 = exclude_sp1,
      priors = priors,
      two_step = TRUE,
      simplify = TRUE
    ),
    batches = 8,
    reps = 5,
    names = tidyselect::any_of(c("priors_scenario", "exclude_sp1"))
  )
)
