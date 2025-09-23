test_that("nmbr works for scalar values", {
  expect_equal(nmbr(12345.6789, accuracy = 1), "12\u202F346")
  expect_equal(nmbr(-0.123456, accuracy = 0.001), "\u22120.123")
  expect_equal(
    nmbr(-12345.6789, accuracy = 0.01, html = TRUE, prefix = "$"),
    "&minus;$12&#x202F;345.68"
  )
  expect_equal(nmbr(-0.123456, scale = 100, suffix = "%"), "\u221212%")
  expect_equal(
    nmbr(123456.789, big.mark = ".", decimal.mark = ",", accuracy = 0.001),
    "123.456,789"
  )
  expect_equal(
    nmbr(
      -123456.789,
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 0.001,
      bold = TRUE
    ),
    "**\u2212123.456,789**"
  )
  expect_equal(
    nmbr(
      -123456.789,
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 0.001,
      italic = TRUE
    ),
    "*\u2212123.456,789*"
  )
  expect_equal(
    nmbr(
      -123456.789,
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 0.001,
      italic = TRUE,
      bold = TRUE
    ),
    "***\u2212123.456,789***"
  )
  expect_equal(nmbr(1.23456789, scale = -1000, accuracy = 0.01), "−1 234.57")
  expect_equal(nmbr(-1.23456789, scale = -1000, accuracy = 0.01), "1 234.57")
  expect_equal(nmbr(numeric()), character())
  expect_equal(nmbr(NA_real_), NA_character_)
  expect_equal(nmbr(NA_real_, na = "NA"), "NA")
})

test_that("nmbr works for vector formatting options", {
  expect_equal(
    nmbr(c(0.123456, -123.456, 123456)),
    c("0", "\u2212123", "123\u202F456")
  )
  expect_equal(
    nmbr(c(0.123456, -123.456, 123456), accuracy = c(0.01, 0.1, 100)),
    c("0.12", "\u2212123.5", "123\u202F500")
  )
  expect_equal(
    nmbr(
      c(0.123456, -123.456, 123456),
      scale = c(100, 1, 1 / 1000),
      suffix = c("%", "", "k"),
      prefix = c("", "$", "")
    ),
    c("12%", "\u2212$123", "123k")
  )
  expect_equal(
    nmbr(
      c(0.123456, -1234.56, 123456),
      accuracy = 0.01,
      big.mark = c("", " ", ","),
      decimal.mark = c("/", ",", ".")
    ),
    c("0/12", "\u22121 234,56", "123,456.00")
  )
  expect_equal(
    nmbr(
      c(0.123456, -1234.56, 123456),
      accuracy = 0.01,
      big.mark = c("", " ", ","),
      decimal.mark = c("/", ",", "."),
      bold = c(TRUE, TRUE, FALSE),
      italic = c(TRUE, FALSE, TRUE)
    ),
    c("***0/12***", "**\u22121 234,56**", "*123,456.00*")
  )
})

test_that("prct works as expected", {
  expect_equal(prct(0.123456), nmbr(0.123456, scale = 100, suffix = "%"))
  expect_equal(prct(0.987654, percent = ""), nmbr(0.987654, scale = 100))
  expect_equal(
    prct(0.987654, bold = TRUE),
    nmbr(0.987654, scale = 100, suffix = "%", bold = TRUE)
  )
  expect_equal(
    prct(0.987654, italic = TRUE),
    nmbr(0.987654, scale = 100, suffix = "%", italic = TRUE)
  )
})

test_that("cmma works as expected", {
  expect_equal(cmma(123456), nmbr(123456, big.mark = ","))
  expect_equal(cmma(987654, comma = "< >"), nmbr(987654))
  expect_equal(
    cmma(123456, bold = TRUE),
    nmbr(123456, big.mark = ",", bold = TRUE)
  )
  expect_equal(
    cmma(123456, italic = TRUE),
    nmbr(123456, big.mark = ",", italic = TRUE)
  )
})

test_that("dllr works as expected", {
  expect_equal(dllr(123456), nmbr(123456, prefix = "$"))
  expect_equal(dllr(987654, dollar = "\u20ac"), nmbr(987654, prefix = "\u20ac"))
  expect_equal(
    dllr(123456, bold = TRUE),
    nmbr(123456, prefix = "$", bold = TRUE)
  )
  expect_equal(
    dllr(123456, italic = TRUE),
    nmbr(123456, prefix = "$", italic = TRUE)
  )
})

test_that("pval works as expected", {
  expect_equal(pval(0.0012345), "0.0012")
  expect_equal(pval(0.00012345, min_p = 0.001), "<0.001")
  expect_equal(
    pval(c(0.0012345, 0.00012345), min_p = 0.001, add_p = TRUE),
    c("p=0.0012", "p<0.001")
  )
})

test_that("nmbr gives appropriate error messages", {
  expect_error(nmbr("123456"), class = "formattr_error_wrong_type")
  expect_error(
    nmbr(123456, accuracy = "a"),
    class = "formattr_error_wrong_type"
  )
  expect_error(nmbr(123456, scale = "a"), class = "formattr_error_wrong_type")
  expect_error(nmbr(123456, prefix = 1), class = "formattr_error_wrong_type")
  expect_error(nmbr(123456, suffix = 1), class = "formattr_error_wrong_type")
  expect_error(nmbr(123456, big.mark = 1), class = "formattr_error_wrong_type")
  expect_error(
    nmbr(123456, decimal.mark = 1),
    class = "formattr_error_wrong_type"
  )
  expect_error(nmbr(123456, bold = 1), class = "formattr_error_wrong_type")
  expect_error(nmbr(123456, italic = "1"), class = "formattr_error_wrong_type")
  expect_error(nmbr(123456, html = "a"), class = "formattr_error_wrong_type")
  expect_error(nmbr(123456, na = 1), class = "formattr_error_wrong_type")
  expect_error(
    nmbr(123456, accuracy = 1:2),
    class = "formattr_error_wrong_length"
  )
  expect_error(nmbr(123456, scale = 1:2), class = "formattr_error_wrong_length")
  expect_error(
    nmbr(123456, prefix = rep("$", 2)),
    class = "formattr_error_wrong_length"
  )
  expect_error(
    nmbr(123456, suffix = rep("%", 2)),
    class = "formattr_error_wrong_length"
  )
  expect_error(
    nmbr(123456, big.mark = rep(",", 2)),
    class = "formattr_error_wrong_length"
  )
  expect_error(
    nmbr(123456, decimal.mark = rep(".", 2)),
    class = "formattr_error_wrong_length"
  )
  expect_error(
    nmbr(123456, bold = rep(TRUE, 2)),
    class = "formattr_error_wrong_length"
  )
  expect_error(
    nmbr(123456, italic = rep(FALSE, 2)),
    class = "formattr_error_wrong_length"
  )
  expect_error(
    pval(c(0.0123, 0.1234), accuracy = c(1, 2)),
    class = "formattr_error_wrong_type"
  )
  expect_error(pval(0.0123, min_p = "a"), class = "formattr_error_wrong_type")
  expect_error(
    pval(0.0123, accuracy = 0.01, min_p = 0.001),
    class = "formattr_error_invalid_min_p"
  )
  expect_error(pval(0.0123, add_p = 1), class = "formattr_error_wrong_type")
})

test_that("create_nmbr works", {
  f <- create_nmbr()
  expect_equal(
    f(c(0.123456, -123.456, 123456)),
    c("0", "\u2212123", "123\u202F456")
  )
  f <- create_nmbr(accuracy = c(0.01, 0.1, 100))
  expect_equal(
    f(c(0.123456, -123.456, 123456)),
    c("0.12", "\u2212123.5", "123\u202F500")
  )
  f <- create_nmbr(
    scale = c(100, 1, 1 / 1000),
    suffix = c("%", "", "k"),
    prefix = c("", "$", "")
  )
  expect_equal(f(c(0.123456, -123.456, 123456)), c("12%", "\u2212$123", "123k"))
  f <- create_nmbr(
    accuracy = 0.01,
    big.mark = c("", " ", ","),
    decimal.mark = c("/", ",", ".")
  )
  expect_equal(
    f(c(0.123456, -1234.56, 123456)),
    c("0/12", "\u22121 234,56", "123,456.00")
  )
})
