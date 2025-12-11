library(testthat)

# ---- Test for Fick's First Law ----
test_that("simple flux computation works", {
  # ficks_law_flux(D = 1, dC = 1, dx = 1) should return -1
  expect_equal(
    ficks_law_flux(D = 1, dC = 1, dx = 1),
    -1
  )
})

# ---- Test for linear concentration profile ----
test_that("linear concentration profile is correct", {
  x <- c(0, 0.5, 1)
  p <- concentration_profile(C1 = 0, C2 = 2, x = x)

  # Expect exact linear interpolation
  expect_equal(
    p,
    c(0, 1, 2)
  )
})

# ---- Stability condition test (explicit scheme) ----
test_that("stability check returns TRUE for stable parameters", {
  # Stability: dt <= dx^2 / (2D)
  # Here: dt = 0.0004, dx = 0.1, D = 0.01
  # dx^2 / (2D) = 0.1^2 / (0.02) = 0.5 → stable
  s <- check_stability(D = 0.01, dx = 0.1, dt = 0.0004)

  expect_true(
    s$ok,
    info = "Stability check should return TRUE for valid parameters."
  )
})

