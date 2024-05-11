# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)

# Set target options:
# tar_option_set(
#   packages = c("dplyr")
# )

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Pipeline
list(
  tar_target(data_files, c("data-raw/Exp1.Rdata", "data-raw/Exp2.Rdata"), format = "file"),
  tar_target(exp1_data, get_data(data_files[1], longgap = 3000)),
  tar_target(exp2_data, get_data(data_files[2], longgap = 6000)),
  tar_target(exp1_data_agg, aggregate_data(exp1_data)),
  tar_target(exp2_data_agg, aggregate_data(exp2_data))
)
