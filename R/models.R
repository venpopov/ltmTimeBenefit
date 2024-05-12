#' Serial Recall Model
#'
#' This function implements the model currently described in the draft on page 19.
#' It gives the predicted recall probability for each item in a set of items.
#'
#' @param setsize The number of items in the set.
#' @param ISI A numeric vector representing the inter-stimulus interval for each item.
#' @param item_in_ltm A logical vector indicating whether each item is in LTM.
#' @param prop The proportion of resources allocated to each item.
#' @param prop_ltm Proportion by which the resources used by LTM items are multiplied.
#' @param tau The threshold for recall probability.
#' @param gain The gain parameter for the recall probability function.
#' @param rate The rate at which resources are recovered.
#' @param r_max The maximum amount of resources.
#'
#' @return A numeric vector representing the recall probability for each item.
#'
#' @details The function uses a simulation approach. It loops over trials
#'
#' @examples
#' serial_recall(setsize = 3, ISI = rep(0.5, 3))
serial_recall <- function(
    setsize, ISI = rep(0.5, setsize), item_in_ltm = rep(TRUE, setsize),
    prop = 0.2, prop_ltm = 0.5, tau = 0.15,
    gain = 25, rate = 0.1, r_max = 1) {
  R <- r_max
  p_recall <- vector("numeric", length = setsize)
  prop_ltm <- ifelse(item_in_ltm, prop_ltm, 1)

  for (item in 1:setsize) {
    # strength of the item and recall probability
    strength <- prop * R
    p_recall[item] <- 1 / (1 + exp(-(strength - tau) * gain))

    # amount of resources consumed by the item
    r_cost <- strength * prop_ltm[item]

    # deplete and recover resources
    R <- R - r_cost + rate * ISI[item]
    R <- min(r_max, R)
  }

  p_recall
}


#' Calculate the deviance of a model
#'
#' This function calculates the deviance of a serial_recall model given a set of parameters and data.
#' The deviance is a measure of how well the model fits the data.
#'
#' @param params A named vector of model parameters
#' @param dat A data frame containing the data
#' @param exclude_sp1 A logical indicating whether to exclude the first item from the likelihood calculation
#'
#' @return The deviance of the model
#'
#' @examples
#' params <- c(prop = 0.5, prop_ltm = 0.3, tau = 0.2, gain = 1, rate = 0.4)
#' data <- data.frame(
#'   ISI = c(100, 200, 300), item_in_ltm = c(TRUE, FALSE, TRUE),
#'   n_correct = c(10, 15, 20), n_total = c(20, 20, 20)
#' )
#' calcdev(params, data)
#'
#' @export
calcdev <- function(params, dat, exclude_sp1 = FALSE) {
  pred <- serial_recall(
    setsize = nrow(dat),
    ISI = dat$ISI,
    item_in_ltm = dat$item_in_ltm,
    prop = params["prop"],
    prop_ltm = params["prop_ltm"],
    tau = params["tau"],
    gain = params["gain"],
    rate = params["rate"]
  )
  log_lik <- dbinom(dat$n_correct, dat$n_total, prob = pred, log = TRUE)
  if (exclude_sp1) {
    log_lik <- log_lik[-1]
  }
  -sum(log_lik)
}


#' Calculate the overall deviance
#'
#' This function calculates the overall deviance for a given set of parameters and data.
#' It splits the dataset by the given column and calculates the deviance for each subset.
#' The overall deviance is the sum of the deviances for each subset.
#'
#' @param params A vector of parameters.
#' @param data A data frame containing the data.
#' @param by The column(s) to split the data by.
#'
#' @return The overall deviance.
#' @export
overall_deviance <- function(params, data, by = c("chunk", "gap"), ...,
                             priors = list()) {
  groups <- interaction(data[, by])
  split_data <- split(data, groups)
  dev <- unlist(lapply(split_data, function(x) calcdev(params, x, ...)))
  out <- sum(dev)

  if (length(priors) > 0) {
    pnames <- names(priors)
    for (i in seq_along(priors)) {
      out <- out - dnorm(params[pnames[i]], mean = priors[[i]]$mean, sd = priors[[i]]$sd, log = TRUE)
    }
  }
  out
}


estimate_model <- function(start, data, two_step = FALSE, priors = list(), simplify = FALSE, ...) {
  # internal helper functions
  constrain_pars <- function(par) {
    par[c("prop", "prop_ltm", "tau", "rate")] <- inv_logit(par[c("prop", "prop_ltm", "tau", "rate")])
    par["gain"] <- inv_logit(par["gain"], lb = 0, ub = 100)
    par
  }

  unconstrain_pars <- function(par) {
    par[c("prop", "prop_ltm", "tau", "rate")] <- logit(par[c("prop", "prop_ltm", "tau", "rate")])
    par["gain"] <- logit(par["gain"], lb = 0, ub = 100)
    par
  }

  fn <- function(par, data, par2 = NULL, ...) {
    par <- c(par, par2)
    par <- constrain_pars(par)
    overall_deviance(par, data, ...)
  }

  start_uc <- unconstrain_pars(start)

  # if two_step is TRUE, nest the optimization (prop, prop_ltm, and rate) within the outer optimization (tau, gain)
  if (two_step) {
    start1 <- start_uc[c("prop", "prop_ltm", "rate")]
    start2 <- start_uc[c("tau", "gain")]

    fn2 <- function(par, data, par2, ...) {
      fit <- optim(
        par = par2,
        fn = fn,
        data = data,
        control = list(maxit = 1e6),
        par2 = par,
        ...
      )
      environment(fn)$fit_inner <- fit
      fit$value
    }
  } else {
    start1 <- start_uc
    start2 <- NULL
    fn2 <- fn
  }


  fit <- optim(
    par = start1,
    fn = fn2,
    data = data,
    control = list(maxit = 1e6),
    par2 = start2,
    priors = priors,
    ...
  )

  est <- fit$par
  convergence <- fit$convergence
  value <- fit$value
  counts <- fit$counts
  if (two_step) {
    est <- c(est, fit_inner$par)
    concergence <- convergence + fit_inner$convergence
    counts <- counts + fit_inner$counts
  }

  # return the estimated parameters
  est <- constrain_pars(est)
  class(est) <- "serial_recall_pars"
  fit <- structure(
    list(
      start = start,
      par = est,
      convergence = convergence,
      counts = counts,
      value = value
    ),
    class = "serial_recall_fit"
  )

  if (simplify) {
    out <- optimfit_to_df(fit)
    out$fit <- list(fit)
    return(out)
  }

  fit
}



predict.serial_recall_pars <- function(object, data, group_by) {
  if (missing(group_by)) {
    pred <- serial_recall(
      setsize = nrow(data),
      ISI = data$ISI,
      item_in_ltm = data$item_in_ltm,
      prop = object["prop"],
      prop_ltm = object["prop_ltm"],
      tau = object["tau"],
      gain = object["gain"],
      rate = object["rate"]
    )
    return(pred)
  }

  by <- do.call(paste, c(data[, group_by], sep = "_"))
  out <- lapply(split(data, by), function(x) {
    x$pred_tmp_col295 <- predict(object, x)
    x
  })
  out <- do.call(rbind, out)
  out <- suppressMessages(dplyr::left_join(data, out))
  out$pred_tmp_col295
}

predict.serial_recall_fit <- function(object, data, group_by) {
  predict(object$par, data, group_by)
}
