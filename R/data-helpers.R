#' Get data object from a file
#'
#' This function safely loads the environment from an Rdata file and returns
#' an object from it.
#'
#' @param path The path to the file containing the Rdata file
#' @param object_name The name of the object to extract from the Rdata file
#' @return The loaded data object.
#' @export
extract_object_from_rdata <- function(path, object_name = "data_an") {
  env <- new.env()
  load(path, envir = env)
  if (!exists(object_name, envir = env)) {
    stop("The object ", object_name, " does not exist in the Rdata file.")
  }
  env[[object_name]]
}

#' Preprocesses the data
#'
#' This function preprocesses the input data by converting the trial column to numeric,
#' transforming the gap column values, and categorizing the itemtype based on serpos values.
#'
#' @param data The input data frame
#' @param longgap The value to use for the long gap in ms.
#'
#' @return The preprocessed data frame
#'
#' @examples
preprocess_data <- function(data, longgap) {
  mutate <- dplyr::mutate
  case_when <- dplyr::case_when

  data |>
    mutate(
      trial = as.numeric(trial),
      gap = ifelse(gap == "short", 500, longgap),
      itemtype = case_when(
        serpos %in% 1:3 ~ "SP1-3",
        serpos %in% 4:6 ~ "SP4-6",
        serpos %in% 7:9 ~ "SP7-9"
      )
    )
}

#' get_data function
#'
#' This function takes a file path as input and returns preprocessed data.
#'
#' @param path The file path to the RData file.
#' @return The preprocessed data.
#' @export
#' @seealso extract_object_from_rdata preprocess_data
get_data <- function(path, ...) {
  data <- extract_object_from_rdata(path)
  preprocess_data(data, ...)
}

#' Aggregate Data
#'
#' This function aggregates the given data by grouping it based on the chunk, gap, and itemtype columns.
#' It then calculates various summary statistics including the total count, the count of correct values,
#' and the proportion of correct values.
#'
#' @param data The input data frame.
#'
#' @return A new data frame with the aggregated data.
#'
#' @import dplyr
#'
#' @examples
#' tar_load(exp1_data)
#' aggregate_data(exp1_data)
#'
#' @export
aggregate_data <- function(data) {
  group_by <- dplyr::group_by
  summarise <- dplyr::summarise

  data |>
    group_by(chunk, gap, itemtype) |>
    summarise(
      n_total = dplyr::n(),
      n_correct = sum(cor),
      p_correct = mean(cor)
    )
}
