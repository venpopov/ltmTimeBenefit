#' Serial Recall Model
#'
#' This function implements the model currently described in the draft on page 19.
#' It gives the predicted recall probability for each item in a set of items.
#'
#' @param setsize The number of items in the set.
#' @param ISI A numeric vector representing the inter-stimulus interval for each item.
#' @param prop The proportion of resources allocated to each item.
#' @param prop_ltm Proportion by which the resources used by LTM items are multiplied.
#' @param tau The threshold for recall probability.
#' @param gain The gain parameter for the recall probability function.
#' @param rate The rate at which resources are recovered.
#' @param r_max The maximum amount of resources.
#'
#' @return A numeric vector representing the recall probability for each item.
#'
#' @examples
#' SerialRecall(setsize = 3, ISI = rep(0.5, 3))
SerialRecall <- function(setsize, ISI, prop = 0.2, prop_ltm = 0.5, tau = 0.15,
                         gain = 25, rate = 0.1, r_max = 1) {
  R <- r_max
  p_recall <- vector("numeric", length = setsize)

  for (item in 1:setsize) {
    # strength of the item and recall probability
    strength <- prop * R
    p_recall[item] <- 1 / (1 + exp(-(strength - tau) * gain))

    # amount of resources consumed by the item
    r_cost <- strength
    if (item == 1) r_cost <- r_cost * prop_ltm

    # deplete and recover resources
    R <- R - r_cost + rate * ISI[item]
    R <- min(r_max, R)
  }

  p_recall
}
