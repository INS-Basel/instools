test_vector <- c(1, 1, NA, 2, 3, 3, 4, 3, 1, NA)

test_that("n_missing() works", {
  expect_equal(n_missing(test_vector), 2)
})


