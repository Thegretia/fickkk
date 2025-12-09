library(testthat)

test_that("flux simple", {
  expect_equal(ficks_law_flux(1, 1, 1), -1)
})

test_that("profile linear", {
  x <- c(0, 0.5, 1)
  p <- concentration_profile(0, 2, x)
  expect_equal(p, c(0,1,2))
})

test_that("stability check", {
  s <- check_stability(D = 0.01, dx = 0.1, dt = 0.0004)
  expect_true(s$ok)
})


