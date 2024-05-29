#' Logit Transformation
#'
#' This function performs a logit transformation on a given variable.
#'
#' @param x The variable to be transformed.
#' @param lb The lower bound of the variable (default is 0).
#' @param ub The upper bound of the variable (default is 1).
#'
#' @return The logit-transformed variable.
#'
#' @examples
#' logit(0.5) # returns 0
#' logit(0.25, lb = 0, ub = 0.5) # returns 0
#' logit(0.75, lb = 0.5, ub = 1) # returns 0
#'
#' @export
logit <- function(x, lb = 0, ub = 1) {
  x <- (x - lb) / (ub - lb)
  log(x / (1 - x))
}

#' Inverse Logit Transformation
#'
#' This function applies the inverse logit transformation to a given value.
#'
#' @param x The input value to be transformed.
#' @param lb The lower bound of the transformed value. Default is 0.
#' @param ub The upper bound of the transformed value. Default is 1.
#'
#' @return The transformed value between the lower and upper bounds.
#'
#' @examples
#' inv_logit(0) # returns 0.5
#' inv_logit(0, lb = 0, ub = 10) # returns 5
#'
#' @export
inv_logit <- function(x, lb = 0, ub = 1) {
  out <- 1 / (1 + exp(-x))
  out * (ub - lb) + lb
}




# A wrapper around optim which unconstains the parameters before optimization
constrain <- function(x, lb, ub) {
  if (length(lb) == 1 && length(ub) == 1 && is.infinite(lb) && is.infinite(ub)) {
    return(x)
  }

  dplyr::case_when(
    is.infinite(lb) & is.infinite(ub) ~ x,
    is.infinite(ub) ~ exp(x) + lb,
    is.infinite(lb) ~ ub - exp(x),
    .default = inv_logit(x, lb = lb, ub = ub)
  )
}

unconstrain <- function(x, lb, ub) {
  if (length(lb) == 1 && length(ub) == 1 && is.infinite(lb) && is.infinite(ub)) {
    return(x)
  }

  dplyr::case_when(
    is.infinite(lb) & is.infinite(ub) ~ x,
    is.infinite(ub) ~ log(x - lb),
    is.infinite(lb) ~ log(ub - x),
    .default = logit(x, lb = lb, ub = ub)
  )
}

optim2 <- function(par, fn, ..., lower = -Inf, upper = Inf, control = list()) {
  fn2 <- function(par, ...) {
    par <- constrain(par, lower, upper)
    fn(par, ...)
  }

  start <- unconstrain(par, lower, upper)
  res <- optim(start, fn2, ..., control = control)
  res$par <- constrain(res$par, lower, upper)
  res
}

#' @export
print.serial_recall_pars <- function(x, ...) {
  class(x) <- NULL
  print(x, ...)
}

#' @export
print.serial_recall_fit <- function(x, ...) {
  class(x) <- NULL
  print(x, ...)
}


paper_params <- function(exp = 1) {
  exp1 <- structure(
    c(prop = 0.21, prop_ltm = 0.55, tau = 0.14, gain = 25, rate = 0.02),
    class = "serial_recall_pars"
  )

  exp2 <- structure(
    c(prop = 0.17, prop_ltm = 0.4, tau = 0.135, gain = 25, rate = 0.025),
    class = "serial_recall_pars"
  )

  switch(exp,
    exp1,
    exp2
  )
}

start_fun <- function(seed = sample(1:1e6)) {
  withr::with_seed(
    seed,
    {
      par <- c(
        prop = runif(1, 0.1, 0.8),
        prop_ltm = runif(1, 0.3, 0.8),
        rate = runif(1, 0.01, 0.3),
        gain = runif(1, 1, 30)
      )
      par["tau"] <- par["prop"] * 0.5
      par
    }
  )
}

start_fun2 <- function(seed = sample(1:1e6)) {
  withr::with_seed(
    seed,
    {
      par <- c(
        prop = runif(1, 0.1, 0.5),
        prop_ltm = runif(1, 0.3, 0.8),
        gain = runif(1, 1, 30)
      )
      par["tau"] <- par["prop"] * 0.5
      par["rate"] <- runif(1, 0.01, par["prop"] / 6)
      par
    }
  )
}


optimfit_to_df <- function(fit) {
  par <- fit$par
  names <- names(par)
  if (is.null(names)) {
    names(par) <- paste0("V", 1:length(par))
  }
  out <- tibble::as_tibble(as.list(par))
  out$deviance <- fit$value
  out$convergence <- fit$convergence
  out
}

rd2md_docs <- function(rd_dir = "man", md_dir = "quarto/reference") {
  rd_files <- list.files(rd_dir, pattern = "\\.Rd$", full.names = TRUE)
  if (!dir.exists(md_dir)) {
    dir.create(md_dir)
  }
  for (rd_file in rd_files) {
    rdtext <- Rd2md::read_rdfile(rd_file, ".")
    mdtext <- Rd2md::as_markdown(rdtext)
    md_file <- gsub("\\.Rd$", ".md", rd_file)
    writeLines(mdtext, file.path(md_dir, basename(md_file)))
  }
}

#' Perform bootstrapped estimation
#'
#' This function performs bootstrapped estimation using the provided data and parameters.
#' It uses the serial recall model for estimation
#'
#' @param data The input data for estimation.
#' @param start The starting values for estimation. If not provided, default values will be used.
#' @param exclude_sp1 Logical value indicating whether to exclude the first item in the data. Default is TRUE.
#' @param growth The growth type for estimation. Default is "asy".
#' @param ... Additional arguments to be passed to the estimation function.
#'
#' @return A data frame containing the estimated model parameters.
#'
#' @examples
#' data <- read.csv("data.csv")
#' boot_est(data)
#'
#' @export
boot_est <- function(data, start, exclude_sp1 = TRUE, growth = "asy", ...) {
  if (missing(start)) {
    start <- c(prop = 0.15, prop_ltm = 0.5, rate = 0.25, gain = 90, tau = 0.12)
  }
  boot_data <- gen_boot_dataset(data)
  est <- estimate_model(start, data = boot_data, exclude_sp1 = exclude_sp1, growth = growth, ...)
  optimfit_to_df(est)
}


#' Execute an expression and save the result to a file or load the result from a file if it already exists.
#'
#' This function allows you to either run an expression or load the result from a file.
#'
#' @param expression The expression to be evaluated or loaded.
#' @param file The file path where the result will be saved or loaded from.
#' @param ... Additional arguments to be passed to the expression.
#' @param force Logical value indicating whether to force the evaluation of the expression, even if the file exists.
#'
#' @return The result of the expression.
#'
#' @examples
#' # Run an expression and save the result to a file
#' file <- tempfile(fileext = ".rds")
#' run_or_load(rnorm(1e7), file)
#' run_or_load(rnorm(1e7), file) # loads the result from the file
#'
#' @export
run_or_load <- function(expression, file, ..., force = FALSE) {
  if (file.exists(file) && !force) {
    res <- readRDS(file)
  } else {
    res <- expression
    saveRDS(res, file)
  }
  res
}
