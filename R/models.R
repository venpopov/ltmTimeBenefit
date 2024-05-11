SerialRecall <- function(setsize, ISI, prop, prop_ltm, tau, gain, rate,
                         r_max = 1) {
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
    R <- min(r_max, R))
  }

  p_recall
}
