# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("tibble") 
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Pipeline
list(
  tar_target(data_files, c("data-raw/Exp1.Rdata", "data-raw/Exp2.Rdata"), format = "file"),
  tar_target(exp1_data, extract_object_from_rdata(data_files[1])),
  tar_target(exp2_data, extract_object_from_rdata(data_files[2]))
)
