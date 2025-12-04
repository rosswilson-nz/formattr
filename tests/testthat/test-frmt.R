test_that("est_ci works as expected", {
  expect_equal(
    est_ci(c(1, 2), c(0.5, 1), c(1.4, 3.1), accuracy = 0.1),
    c('1.0 (0.5 to 1.4)', '2.0 (1.0 to 3.1)')
  )
  expect_equal(
    est_ci(
      c(0.01, 0.02),
      c(0.005, 0.01),
      c(0.014, 0.031),
      accuracy = 0.1,
      scale = 100,
      suffix = "%"
    ),
    c('1.0% (0.5% to 1.4%)', '2.0% (1.0% to 3.1%)')
  )
  expect_equal(
    est_ci(
      c(0.01, 0.02),
      c(0.005, 0.01),
      c(0.014, 0.031),
      accuracy = 0.1,
      scale = 100,
      suffix = "%",
      format_paren = FALSE
    ),
    c('1.0% (0.5 to 1.4)', '2.0% (1.0 to 3.1)')
  )
  expect_equal(
    est_ci(c(100, 200), c(50, 100), c(140, 310), prefix = "$"),
    c('$100 ($50 to $140)', '$200 ($100 to $310)')
  )
  expect_equal(
    est_ci(c(1, 2), c(0.5, 1), c(1.4, 3.1), c(0.023, 0.0023), accuracy = 0.1),
    c('1.0 (0.5 to 1.4) **', '2.0 (1.0 to 3.1) ***')
  )
  expect_equal(
    est_ci(
      c(1, 2),
      c(0.5, 1),
      c(1.4, 3.1),
      c(0.0023, 0.00023),
      accuracy = 0.1,
      stars = FALSE,
      min_p = 0.001
    ),
    c('1.0 (0.5 to 1.4) (p=0.0023)', '2.0 (1.0 to 3.1) (p<0.001)')
  )
  expect_equal(
    est_ci(
      c(1, 2),
      c(0.5, NA),
      c(1.4, NA),
      c(0.0023, 0.00023),
      accuracy = 0.1,
      stars = FALSE,
      min_p = 0.001
    ),
    c('1.0 (0.5 to 1.4) (p=0.0023)', '2.0 (p<0.001)')
  )
  expect_equal(
    est_ci(
      c(1, 2),
      c(0.5, 1),
      c(1.4, 3.1),
      c(0.0023, NA),
      accuracy = 0.1,
      stars = FALSE,
      min_p = 0.001
    ),
    c('1.0 (0.5 to 1.4) (p=0.0023)', '2.0 (1.0 to 3.1)')
  )
})

test_that("n_percent works as expected", {
  expect_equal(
    n_percent(c(1, 2), c(0.33333, 0.66667), accuracy = 0.1),
    c('1 (33.3%)', '2 (66.7%)')
  )
  expect_equal(
    n_percent(
      c(1, 2),
      c(0.33333, 0.66667),
      accuracy = 0.1,
      format_paren = FALSE
    ),
    c('1 (33.3)', '2 (66.7)')
  )
  expect_equal(
    n_percent(
      c(123456, 234567),
      scale = 1 / 1000,
      suffix = "k",
      accuracy = 0.1
    ),
    c('123k (34.5%)', '235k (65.5%)')
  )
})

test_that("mean_sd works as expected", {
  expect_equal(
    mean_sd(c(1, 2), c(0.33333, 0.66667), accuracy = 0.1),
    c('1.0 (0.3)', '2.0 (0.7)')
  )
  expect_equal(
    mean_sd(c(100, 200), c(62.3, 111.8), prefix = "$"),
    c('$100 ($62)', '$200 ($112)')
  )
  expect_equal(
    mean_sd(c(100, 200), c(62.3, 111.8), prefix = "$", format_paren = FALSE),
    c('$100 (62)', '$200 (112)')
  )
  expect_equal(
    mean_sd(c(100, 200), c(62.3, 111.8), suffix = "\u20ac"),
    c('100\u20ac (62\u20ac)', '200\u20ac (112\u20ac)')
  )
  expect_equal(
    mean_sd(
      c(100, 200),
      c(62.3, 111.8),
      suffix = "\u20ac",
      format_paren = FALSE
    ),
    c('100\u20ac (62)', '200\u20ac (112)')
  )
})

test_that("coef_se works as expected", {
  expect_equal(
    coef_se(c(1, 2), c(0.5, 1), accuracy = 0.1),
    c('1.0 (0.5)', '2.0 (1.0)')
  )
  expect_equal(
    coef_se(c(1, 2), c(0.5, 1), c(0.023, 0.0023), accuracy = 0.1),
    c('1.0 (0.5) **', '2.0 (1.0) ***')
  )
  expect_equal(
    coef_se(
      c(1, 2),
      c(0.5, 1),
      c(0.0023, 0.00023),
      accuracy = 0.1,
      stars = FALSE,
      min_p = 0.001
    ),
    c('1.0 (0.5) (p=0.0023)', '2.0 (1.0) (p<0.001)')
  )
  expect_equal(
    coef_se(
      c(1, 2),
      p = c(0.0023, 0.00023),
      accuracy = 0.1,
      stars = FALSE,
      min_p = 0.001
    ),
    c('1.0 (p=0.0023)', '2.0 (p<0.001)')
  )
  expect_equal(
    coef_se(
      c(1, 2),
      p = c(0.0023, 0.00023),
      accuracy = 0.1,
      stars = FALSE,
      min_p = 0.001,
      add_p = FALSE
    ),
    c('1.0 (0.0023)', '2.0 (<0.001)')
  )
})

test_that("frmt functions give appropriate error messages", {
  expect_error(
    mean_sd(1, 2, format_paren = 1),
    class = "formattr_error_wrong_type"
  )
  expect_error(
    n_percent(1, 2, format_paren = 1),
    class = "formattr_error_wrong_type"
  )
  expect_error(est_ci(1, 2, 3, .sep = 1), class = "formattr_error_wrong_type")
  expect_error(est_ci(1, 2, 3, stars = 1), class = "formattr_error_wrong_type")
  expect_error(
    est_ci(1, 2, 3, format_paren = 1),
    class = "formattr_error_wrong_type"
  )
  expect_error(coef_se(1, 2, stars = 1), class = "formattr_error_wrong_type")
})
