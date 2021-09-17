test_that("nmbr works for scalar values", {
  expect_equal(nmbr(12345.6789, accuracy = 1), "12&#x202F;346")
  expect_equal(nmbr(-0.123456, accuracy = 0.001), "&minus;0.123")
  expect_equal(nmbr(-12345.6789, accuracy = 0.01, html = FALSE, prefix = "$"),
               "\u2212$12\u202F345.68")
  expect_equal(nmbr(-0.123456, scale = 100, suffix = "%"), "&minus;12%")
  expect_equal(nmbr(123456.789, big.mark = ".", decimal.mark = ",", accuracy = 0.001),
               "123.456,789")
  expect_equal(nmbr(numeric()), character())
  expect_equal(nmbr(NA_real_), NA_character_)
  expect_equal(nmbr(NA_real_, na = "NA"), "NA")
})

test_that("nmbr works for vector formatting options", {
  expect_equal(nmbr(c(0.123456, -123.456, 123456)), c("0", "&minus;123", "123&#x202F;456"))
  expect_equal(nmbr(c(0.123456, -123.456, 123456), accuracy = c(0.01, 0.1, 100)),
               c("0.12", "&minus;123.5", "123&#x202F;500"))
  expect_equal(
    nmbr(c(0.123456, -123.456, 123456), scale = c(100, 1, 1/1000),
         suffix = c("%", "", "k"), prefix = c("", "$", "")),
    c("12%", "&minus;$123", "123k")
  )
  expect_equal(
    nmbr(c(0.123456, -1234.56, 123456), accuracy = 0.01,
         big.mark = c("", " ", ","), decimal.mark = c("/", ",", ".")),
    c("0/12", "&minus;1 234,56", "123,456.00")
  )
})
