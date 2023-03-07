test_that("formatting works without bigmark", {
  vec <- c(1L, 20L, 300L, 4000L, 50000L)
  expect_equal(
    journal_format(vec),
    c("    1",
      "   20",
      "  300",
      " 4000",
      "50,000"))
})


test_that("formatting works with (silly!!) bigmark", {
  vec <- c(1L, 20L, 300L, 4000L, 50000L)
  expect_equal(
    journal_format(vec, big.mark = ";;"),
    c("    1",
      "   20",
      "  300",
      " 4000",
      "50;;000"))
})

test_that("formatting fails with characters", {
  test <- c("test")
  expect_error(
    journal_format(test, big.mark = ",")
  )
})
