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
