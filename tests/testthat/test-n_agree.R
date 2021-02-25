# create a test object
vec <- c(0, 1, 1, 0, NA_real_)

test_that("n_agree works with binary vector and default cutoff", {
  expect_equal(n_agree(vec), 2)
})

test_that("n_agree works with binary vector and specified cutoff", {
  expect_equal(n_agree(vec, cutoff = 1), 2)
})

# second test object
vec2 <- c(1, 3, 4, 4, NA_real_)

test_that("n_agree works with selected cutoff", {
  expect_equal(n_agree(vec2, cutoff = c(3, 4)), 3)
})
