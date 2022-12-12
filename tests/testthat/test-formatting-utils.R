test_that("sf works", {
  expect_equal(sf(123456.789, 8), 0.01)
  expect_equal(sf(123.456789, 2), 10)
  expect_equal(sf(c(123456.789, 123.456789), 8, dp_min = 3), c(0.001, 0.00001))
  expect_equal(sf(c(123456.789, 123.456789), 5, dp_max = 0), c(10, 1))
  expect_equal(sf(c(123456.789, 123.456789), c(3, 6)), c(1000, 0.001))
})

test_that("dp works", {
  expect_equal(dp(4), 0.0001)
  expect_equal(dp(-2), 100)
  expect_equal(dp(c(2, 1, 0, -1, -2)), c(0.01, 0.1, 1, 10, 100))
})
