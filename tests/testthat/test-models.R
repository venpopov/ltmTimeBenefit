source("R/models.R")

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
