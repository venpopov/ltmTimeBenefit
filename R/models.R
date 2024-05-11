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


calcdev <- function(params, dat) {
  pred <- serial_recall(
    setsize = nrow(dat),
    ISI = dat$ISI,
    item_in_ltm = dat$item_in_ltm,
    prop = inv_logit(params["prop"]),
    prop_ltm = inv_logit(params["prop_ltm"]),
    tau = inv_logit(params["tau"]),
    gain = params["gain"],
    rate = inv_logit(params["rate"])
  )
  log_lik <- dbinom(dat$n_correct, dat$n_total, prob = pred, log = TRUE)
  -sum(log_lik)
}
