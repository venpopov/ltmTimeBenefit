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
