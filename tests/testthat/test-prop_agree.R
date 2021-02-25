# test object
vec <- c(0, 1, 1, NA_real_, 0)

test_that("prop_agree works with binary vector and default cutoff", {
  expect_equal(prop_agree(vec), .5)
})

test_that("prop_agree works with binary vector and specified cutoff", {
  expect_equal(prop_agree(vec, cutoff = 1), .5)
})

#second test object
vec2 <- c(1, 3, 4, NA_real_, 4)

test_that("prop_agree works with selected cutoff - all values present", {
  expect_equal(prop_agree(vec2, cutoff = c(3, 4)), .75)
})

test_that("prop_agree works with selected cutoff - not all values present", {
  expect_equal(prop_agree(vec2, cutoff = c(2, 3, 4)), .75)
})

#restrict level tests
# open vector
vec2 <- c(1, 0, 0, NA_real_, 0, 1, 0, 0, NA_real_, 0)

test_that("prop_agree outputs NA when restrict_level < n_valid - binary vector", {
  expect_equal(prop_agree(vec2, cutoff = 1, restrict_level = 10), NA_integer_)
})

# open vector
vec2 <- c(1, 3, 4, NA_real_, 4, 1, 3, 4, NA_real_, 4)

test_that("prop_agree outputs NA when restrict_level < n_valid - open vector", {
  expect_equal(prop_agree(vec2, cutoff = c(2, 3, 4), restrict_level = 10), NA_integer_)
})


# ensure error message is given
vec_char <- c("1", "2", "2", NA_character_, "1")

test_that("gives warning when not integer", {
  expect_error(prop_agree(vec_char))
})
