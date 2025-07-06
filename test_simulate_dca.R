# Tests for simulate_dca using testthat
library(testthat)
source("dca.R")

# Helper: create a simple price matrix (n_days x n_paths)
make_price_matrix <- function(n_days = 12, n_paths = 2, start = 100, step = 1) {
  # Each path will have a different random walk, but always positive prices
  set.seed(42) # for reproducibility
  mat <- matrix(NA, nrow = n_days, ncol = n_paths)
  for (j in 1:n_paths) {
    changes <- rnorm(n_days - 1, mean = step, sd = abs(step) * 0.5 + 0.1)
    prices <- c(start, start + cumsum(changes))
    mat[, j] <- prices
  }
  mat[mat < 1] <- 1 # ensure no negative or zero prices
  mat
}

test_that("simulate_dca returns correct output structure", {
  P <- make_price_matrix()
  res <- simulate_dca(P)
  expect_type(res, "list")
  expect_true(all(c("wealth", "EU", "CE_ratio", "CE_absolute") %in% names(res)))
})

test_that("simulate_dca is deterministic for fixed input", {
  P2 <- make_price_matrix(n_days = 12, n_paths = 1, start = 100, step = 0)
  res2 <- simulate_dca(P2, total = 1200, months = 12, trades_per_month = 1, gamma = 1)
  expect_equal(as.numeric(res2$wealth), c(
    100.677783732717, 100.539947642221, 100.596676628479, 100.560189784412,
    100.496663491094, 100.456125321729, 100.466763834337, 100.315452634301,
    100.324915116593, 100.123531723209, 100.129776701729, 100
  ), tolerance = 1e-8)
  expect_equal(as.numeric(res2$CE_absolute), 100.3904, tolerance = 1e-3)
})

test_that("simulate_dca handles edge case: 1 month, 1 trade", {
  P2 <- make_price_matrix(n_days = 12, n_paths = 1, start = 100, step = 0)
  res3 <- simulate_dca(P2, total = 100, months = 1, trades_per_month = 1, gamma = 1)
  expect_equal(as.numeric(res3$wealth), 100.6778, tolerance = 1e-3)
})

test_that("simulate_dca handles gamma != 1 (CRRA utility)", {
  P2 <- make_price_matrix(n_days = 12, n_paths = 1, start = 100, step = 0)
  res4 <- simulate_dca(P2, total = 100, months = 1, trades_per_month = 1, gamma = 2)
  expect_type(res4$EU, "double")
})

test_that("simulate_dca handles trades_per_month > 1", {
  P3 <- make_price_matrix(n_days = 12, n_paths = 1, start = 100, step = 1)
  res5 <- simulate_dca(P3, total = 1200, months = 2, trades_per_month = 3, gamma = 1)
  expect_type(res5$wealth, "double")
})
