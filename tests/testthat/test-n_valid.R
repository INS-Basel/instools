test_vector <- c(1, 1, NA_real_, 2, 3, 3, 4, 3, 1, NA_real_)

test_that("n_valid works", {
  expect_equal(n_valid(test_vector), 8)
})
