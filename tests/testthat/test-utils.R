test_that("has_names works", {
  expect_equal(has_names(1:3), FALSE)
  expect_equal(has_names(setNames(1:3, "")), FALSE)
  expect_equal(has_names(setNames(1:3, "a")), TRUE)
})

test_that("%||% works", {
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
  expect_null(NULL %||% NULL)
})
