test_that("serial_recall works", {
  out <- serial_recall(
    setsize = 3, ISI = c(0.5, 0.5, 0.5),
    prop = 0.2, prop_ltm = 0.5, tau = 0.14, gain = 25, rate = 0.1, r_max = 1
  )
  expect_equal(length(out), 3)
  expect_true(all(out >= 0 & out <= 1))
  expect_true(all(diff(out) < 0))
  out
})


test_that("calcdev works", {
  ISI <- c(0.5, 0.5, 0.5)
  item_in_ltm <- rep(TRUE, 3)

  probs <- serial_recall(
    setsize = 3, ISI = ISI, item_in_ltm = item_in_ltm,
    prop = 0.2, prop_ltm = 0.5, tau = 0.14, gain = 25, rate = 0.1, r_max = 1
  )

  n_trials <- 10000
  n_correct <- rbinom(3, size = n_trials, prob = probs)
  dat <- data.frame(n_total = rep(n_trials, 3), n_correct = n_correct, ISI = ISI, item_in_ltm = item_in_ltm)

  params <- c(
    prop = logit(0.2),
    prop_ltm = logit(0.5),
    tau = logit(0.14),
    gain = logit(25, 0, 100),
    rate = logit(0.1)
  )

  parscales <- c(
    prop = 1,
    prop_ltm = 1,
    tau = 1,
    gain = 0.1,
    rate = 1
  )

  out <- calcdev(params, dat)
  expect_true(length(out) == 1)
  expect_true(is.numeric(out))

  # recover parameters?
  fit <- optim(
    par = params,
    fn = \(x) {
      x <- inv_logit(x, lb = c(0, 0, 0, 0, 0), ub = c(1, 1, 1, 100, 1))
      calcdev(x, dat)
    },
    control = list(maxit = 1e6, parscale = parscales)
  )

  est_pars <- c(
    inv_logit(fit$par["prop"]),
    inv_logit(fit$par["prop_ltm"]),
    inv_logit(fit$par["tau"]),
    inv_logit(fit$par["gain"], 0, 100),
    inv_logit(fit$par["rate"])
  )

  serial_recall(
    setsize = 3, ISI = ISI, item_in_ltm = item_in_ltm,
    prop = est_pars["prop"], prop_ltm = est_pars["prop_ltm"], tau = est_pars["tau"],
    gain = est_pars["gain"], rate = est_pars["rate"], r_max = 1
  )
})
