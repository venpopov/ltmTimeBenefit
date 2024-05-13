# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(crew)
library(purrr)
library(dplyr)

# Set target options:
tar_option_set(
  controller = crew_controller_local(workers = 10)
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# use quote because of https://github.com/ropensci/tarchetypes/discussions/105
sim1_hyperparameters <- tidyr::expand_grid(
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

# Pipeline
list(
  # Data preprocessing
  tar_target(data_files, c("data-raw/Exp1.Rdata", "data-raw/Exp2.Rdata"), format = "file", deployment = "main"),
  tar_target(exp1_data, get_data(data_files[1], longgap = 3000), deployment = "main"),
  tar_target(exp2_data, get_data(data_files[2], longgap = 6000), deployment = "main"),
  tar_target(exp1_data_agg, aggregate_data(exp1_data), deployment = "main"),
  tar_target(exp2_data_agg, aggregate_data(exp2_data), deployment = "main"),

  # Models
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
  )
)
