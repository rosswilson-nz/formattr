test_that("stars works as expected", {
  expect_equal(
    sstars(seq(0.005, 0.105, by = 0.01), levels = c("*" = 0.1, "**" = 0.05, "***" = 0.01)),
    c("***", rep("**", 4), rep("*", 5), "")
  )
  expect_equal(
    sstars(c(0.0009, 0.001, 0.009, 0.01, 0.04, 0.05, 0.09, 0.1, 0.2),
           levels = c("\u2020" = 0.1, "*" = 0.05, "**" = 0.01, "***" = 0.001)),
    c("***", rep(c("**", "*", "\u2020", ""), each = 2))
  )
})

test_that("stars gives appropriate error messages", {
  expect_error(sstars("a"), class = "formattr_error_wrong_type")
  expect_error(sstars(0.005, c("*" = "a", "**" = "b", "***" = "c")),
               class = "formattr_error_wrong_type")
  expect_error(sstars(0.005, c(0.1, 0.05, 0.01)), class = "formattr_error_wrong_type")
})
