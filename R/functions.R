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
#'
#' @return The preprocessed data frame
#'
#' @examples
preprocess_data <- function(data) {
  mutate <- dplyr::mutate
  case_when <- dplyr::case_when

  data |> 
    mutate(trial = as.numeric(trial),
           gap = ifelse(gap == "short", "500 ms", "3000 ms"),
           itemtype = case_when(
             serpos %in% 1:3 ~ "SP1-3",
             serpos %in% 4:6 ~ "SP4-6",
             serpos %in% 7:9 ~ "SP7-9"
           ))
}

get_data <- function(path) {
  data <- extract_object_from_rdata(path)
  preprocess_data(data)
}