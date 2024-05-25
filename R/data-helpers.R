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

# TODO: add an experiment label
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

  data <- data |>
    mutate(
      trial = as.numeric(trial),
      itemtype = case_when(
        serpos %in% 1:3 ~ "SP1-3",
        serpos %in% 4:6 ~ "SP4-6",
        serpos %in% 7:9 ~ "SP7-9"
      )
    )

  if ("gap" %in% colnames(data)) {
    data$gap <- ifelse(data$gap == "short", 500, longgap)
  } else {
    data$gap <- data$gapdur
  }

  # remove bad subjects
  bad_subj_id <- get_bad_subj_id(data)
  data[!(data$id %in% bad_subj_id), ]
}

get_bad_subj_id <- function(data) {
  data |>
    dplyr::group_by(id) |>
    dplyr::mutate(subj_acc = mean(cor)) |>
    dplyr::filter(subj_acc <= 0.1) |>
    dplyr::pull(id) |>
    unique()
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
  data |>
    dplyr::group_by(chunk, gap, itemtype) |>
    dplyr::summarise(
      n_total = dplyr::n(),
      n_correct = sum(cor),
      p_correct = mean(cor)
    ) |>
    dplyr::group_by(chunk, gap) |>
    dplyr::mutate(
      ISI = c(unique(gap) / 1000, rep(0.5, dplyr::n() - 1)),
      item_in_ltm = ifelse(itemtype == "SP1-3", chunk == "known", FALSE)
    ) |>
    dplyr::ungroup()
}

# temporary - this only works if the data is already loaded
aggregate_by_subject <- function(data) {
  suppressMessages(
    data |>
      dplyr::group_by(id) |>
      tidyr::nest() |>
      dplyr::mutate(data = purrr::map(data, aggregate_data)) |>
      tidyr::unnest(data) |>
        dplyr::ungroup()
  ) 
}


#' Generate a bootstrapped dataset
#'
#' This function takes in a dataset and generates a bootstrapped dataset by randomly sampling
#' unique IDs with replacement. For each ID and condition, it calculates the proportion of correct
#' responses and then generates a new dataset by sampling from a binomial distribution with the
#' calculated proportion of correct responses.
#'
#' @param data The input dataset
#'
#' @return A bootstrapped dataset with aggregated summary statistics
#'
#' @examples
#' data <- read.csv("data.csv")
#' boot_data <- gen_boot_dataset(data)
#'
#' @import dplyr
#' @importFrom stats rbinom
#' @export
gen_boot_dataset <- function(data) {
  suppressMessages({
    unique_ids <- unique(data$id)
    boot_ids <- sample(unique_ids, replace = TRUE)
    boot_data <- data %>% filter(id %in% boot_ids)
    boot_agg_subj <- boot_data |>
      group_by(id) |>
      do({
        aggregate_data(.)
      })
    boot2lvl <- boot_agg_subj |>
      mutate(
        n_correct = rbinom(n(), size = n_total, prob = p_correct),
        p_correct = n_correct / n_total
      )
    boot_agg <- boot2lvl |>
      group_by(chunk, gap, itemtype, ISI, item_in_ltm) |>
      summarize(
        p_correct = mean(p_correct),
        n_total = sum(n_total),
        n_correct = sum(n_correct)
      )
  })
  boot_agg
}
