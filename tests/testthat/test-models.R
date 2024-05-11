test_that("SerialRecall works", {
  out <- SerialRecall(
    setsize = 3, ISI = c(0.5, 0.5, 0.5),
    prop = 0.2, prop_ltm = 0.5, tau = 0.14, gain = 25, rate = 0.1, r_max = 1
  )
  expect_equal(length(out), 3)
  expect_true(all(out >= 0 & out <= 1))
  expect_true(all(diff(out) < 0))
  out
})


test_that("calcdev works", {
  probs <- SerialRecall(
    setsize = 3, ISI = c(0.5, 0.5, 0.5),
    prop = 0.2, prop_ltm = 0.5, tau = 0.14, gain = 25, rate = 0.1, r_max = 1
  )

  n_trials <- 100
  n_correct <- rbinom(3, size = n_trials, prob = probs)
  dat <- data.frame(n_total = rep(n_trials, 3), n_correct = n_correct)

  params <- c(
    prop = logit(0.2),
    prop_ltm = logit(0.5),
    tau = logit(0.14),
    gain = 25,
    rate = log(0.1)
  )

  parscales <- c(
    prop = 1,
    prop_ltm = 1,
    tau = 1,
    gain = 0.1,
    rate = 1
  )

  out <- calcdev(params, setsize = 3, ISI = c(0.5, 0.5, 0.5), dat)
  expect_true(length(out) == 1)
  expect_true(is.numeric(out))

  # recover parameters?
  fit <- optim(
    par = params,
    fn = calcdev,
    setsize = 3, ISI = c(0.5, 0.5, 0.5), dat = dat,
    control = list(maxit = 1e6, parscale = parscales)
  )

  est_pars <- c(
    inv_logit(fit$par["prop"]),
    inv_logit(fit$par["prop_ltm"]),
    inv_logit(fit$par["tau"]),
    fit$par["gain"],
    exp(fit$par["rate"])
  )

  SerialRecall(
    setsize = 3, ISI = c(0.5, 0.5, 0.5),
    prop = est_pars["prop"], prop_ltm = est_pars["prop_ltm"], tau = est_pars["tau"],
    gain = est_pars["gain"], rate = est_pars["rate"], r_max = 1
  )
})
