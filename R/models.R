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
#' @param prop_prim The output interference primacy parameter
#' @param r_max The maximum amount of resources.
#' @param lambda The exponent converting resources to strength.
#' @param growth The growth function for resource recovery. Either 'linear' or 'asy'.
#'
#' @return A numeric vector representing the recall probability for each item.
#'
#' @details The function uses a simulation approach. It loops over trials
#'
#' @examples
#' serial_recall(setsize = 3, ISI = rep(0.5, 3))
serial_recall <- function(
    setsize, ISI = rep(0.5, setsize), item_in_ltm = rep(TRUE, setsize),
    prop = 0.2, prop_ltm = 0.5, tau = 0.15, gain = 25, rate = 0.1, prop_prim = 1,
    r_max = 1, lambda = 1, growth = "linear", ...) {
  R <- r_max
  strength <- rep(0, setsize)
  prop_ltm <- item_in_ltm * prop_ltm + 1 - item_in_ltm

  for (item in 1:setsize) {
    # strength of the item and recall probability
    strength[item] <- (prop * R)^lambda
    r_cost <- strength[item]^(1 / lambda) * prop_ltm[item]
    R <- R - r_cost

    # recover resources
    R <- switch(growth,
      "linear" = min(r_max, R + rate * ISI[item]),
      "asy" = R + (r_max - R) * (1 - exp(-rate * ISI[item]))
    )
  }

  strength <- strength * prop_prim^(seq_along(strength) - 1) # output interference
  p_recall <- 1 / (1 + exp(-(strength - tau) * gain))
  list(strength = strength, p_recall = p_recall)
}



#' Calculate the deviance of a model
#'
#' This function calculates the deviance of a serial_recall model given a set of parameters and data.
#' The deviance is a measure of how well the model fits the data.
#'
#' @param params A named vector of model parameters
#' @param dat A data frame containing the data
#' @param exclude_sp1 A logical indicating whether to exclude the first item from the likelihood calculation
#' @param ... Additional arguments to pass to `serial_recall()`
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
calcdev <- function(params, dat, exclude_sp1 = FALSE, ...) {
  pred <- rlang::inject(serial_recall(
    setsize = nrow(dat),
    ISI = dat$ISI,
    item_in_ltm = dat$item_in_ltm,
    !!!params,
    ...
  ))
  log_lik <- dbinom(dat$n_correct, dat$n_total, prob = pred$p_recall, log = TRUE)
  if (exclude_sp1) {
    log_lik <- log_lik[-1]
  }
  -2 * sum(log_lik)
}


#' Calculate the overall deviance
#'
#' This function calculates the overall deviance for a given set of parameters and data.
#' It calculates the deviance for each list entry of split_data and then sums them
#'
#' @param params A vector of parameters.
#' @param split_data a data.frame split by `split()`
#' @param ... additional arguments to pass to calcdev()
#' @param priors a named list
#'
#' @return The overall deviance.
#' @export
overall_deviance <- function(params, split_data, ..., priors = list()) {
  out <- unlist(lapply(split_data, function(x) calcdev(params, x, ...)))
  out <- sum(out)

  if (length(priors) > 0) {
    pnames <- names(priors)
    for (i in seq_along(priors)) {
      out <- out - dnorm(params[pnames[i]], mean = priors[[i]]$mean, sd = priors[[i]]$sd, log = TRUE)
    }
  }
  out
}

is_logit_par <- function(par) {
  par %in% c("prop", "prop_ltm", "tau", "rate", "prop_prim")
}


estimate_model <- function(
    start, data, two_step = FALSE, priors = list(),
    simplify = FALSE, group_by = c("gap", "chunk"), 
    fixed_params = NULL, iter = 1, control = list(maxit = 1e6), ...) {

  constrain_pars <- function(par) {
    par_names <- names(par)
    par[is_logit_par(par_names)] <- inv_logit(par[is_logit_par(par_names)])
    if ("gain" %in% par_names) par["gain"] <- inv_logit(par["gain"], lb = 0, ub = 100)
    par
  }

  unconstrain_pars <- function(par) {
    par_names <- names(par)
    par[is_logit_par(par_names)] <- logit(par[is_logit_par(par_names)])
    if ("gain" %in% par_names) par["gain"] <- logit(par["gain"], lb = 0, ub = 100)
    par
  }

  fn <- function(par, data, par2 = NULL, fixed_params = NULL, ...) {
    par <- c(constrain_pars(c(par, par2)))
    rlang::inject(overall_deviance(par, split_data = data, !!!fixed_params, ...))
  }

  start_uc <- unconstrain_pars(start)

  # if two_step is TRUE, nest the optimization (prop, prop_ltm, and rate) within the outer optimization (tau, gain)
  if (two_step) {
    start1 <- start_uc[!(names(start_uc) %in% c("tau", "gain"))]
    start2 <- start_uc[names(start_uc) %in% c("tau", "gain")]

    fn2 <- function(par, data, par2, fixed_params = NULL, ...) {
      fit <- optim(
        par = par2,
        fn = fn,
        data = data,
        par2 = par,
        fixed_params = fixed_params,
        control = list(reltol = 1e-6),
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

  if (!is.null(group_by)) {
    groups <- do.call(paste0, data[, group_by])
  } else {
    groups <- rep(1, nrow(data))
  }

  split_data <- split(data, groups)

  fit <- optim(
    par = start1,
    fn = fn2,
    data = split_data,
    control = control,
    par2 = start2,
    priors = priors,
    fixed_params = fixed_params,
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
      fixed_params = fixed_params,
      convergence = convergence,
      counts = counts,
      value = value
    ),
    class = "serial_recall_fit",
    data = data,
    group_by = group_by
  )

  if (iter > 1) {
    fit <- tryCatch(
      {
        estimate_model(
          start = est,
          data = data,
          two_step = two_step,
          priors = priors,
          simplify = simplify,
          group_by = group_by,
          fixed_params = fixed_params,
          iter = iter - 1,
          control = control,
          ...
        )
      },
      error = function(e) {
        fit
      }
    )
    fit$start <- start
    fit$iter <- iter
  }


  if (simplify && iter == 1) {
    out <- optimfit_to_df(fit)
    out$fit <- list(fit)
    return(out)
  }

  fit
}


#' @export
predict.serial_recall_pars <- function(object, data, group_by, type = "p_recall", ...) {
  if (missing(group_by) || is.null(group_by)) {
    sim <- rlang::inject(serial_recall(
      setsize = nrow(data),
      ISI = data$ISI,
      item_in_ltm = data$item_in_ltm,
      !!!object,
      ...
    ))
    return(sim[[type]])
  }

  by <- do.call(paste, c(data[, group_by], sep = "_"))
  out <- lapply(split(data, by), function(x) {
    x$.pred_tmp <- predict(object, x, type = type, ...)
    x
  })
  out <- do.call(rbind, out)
  out <- suppressMessages(dplyr::left_join(data, out))
  out$.pred_tmp
}


#' @export
predict.serial_recall_fit <- function(object, newdata, group_by, ...) {
  if (missing(newdata)) {
    newdata <- attr(object, "data")
  }
  if (missing(group_by)) {
    group_by <- attr(object, "group_by")
  }
  rlang::inject(
    predict(
      object = object$par,
      data = newdata, 
      group_by = group_by, 
      !!!object$fixed_params,
      ...
    )
  )
}
