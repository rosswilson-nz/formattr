df <- tibble::tibble(a = 1:12, b = rep(c("b1", "b2", "b3"), c(4, 5, 3)),
                     c1 = rep(0:1, c(5, 7)), c2 = rep(0:1, c(4, 8)),
                     d = rep(c("d0", "d1"), 6))

test_that("continuous_characteristics_table works", {
  expect_equal(
    continuous_characteristics_table(
      df, NULL, "a", "A", rlang::quo(formattr::mean_sd(.mean, .sd, accuracy = 0.1)), NULL
    ),
    tibble::tibble(`Patient characteristic` = "A", value = "6.5 (3.6)")
  )
  expect_equal(
    continuous_characteristics_table(
      df, "d", "a", "A", rlang::quo(formattr::mean_sd(.mean, .sd, accuracy = 0.1)), "Total"
    ),
    tibble::tibble(`Patient characteristic` = "A", d0 = "6.0 (3.7)",
                   d1 = "7.0 (3.7)", Total = "6.5 (3.6)")
  )
})

test_that("discrete_characteristics_table works", {
  expect_equal(
    discrete_characteristics_table(df, NULL, "b", "B",
                                   rlang::quo(formattr::n_percent(.n, .proportion)), NULL),
    tibble::tibble(`Patient characteristic` = c("B", "&emsp;b1", "&emsp;b2", "&emsp;b3"),
                   value = c(NA, '4 (33%)', '5 (42%)', '3 (25%)'))
  )
  expect_equal(
    discrete_characteristics_table(df, "d", "b", "B",
                                   rlang::quo(formattr::n_percent(.n, .proportion)), "Total"),
    tibble::tibble(`Patient characteristic` = c("B", "&emsp;b1", "&emsp;b2", "&emsp;b3"),
                   d0 = c(NA, '2 (33%)', '3 (50%)', '1 (17%)'),
                   d1 = c(NA, '2 (33%)', '2 (33%)', '2 (33%)'),
                   Total = c(NA, '4 (33%)', '5 (42%)', '3 (25%)'))
  )
})

test_that("multiresponse_characteristics_table works", {
  expect_equal(
    multiresponse_characteristics_table(df, NULL, c(C1 = "c1", C2 = "c2"), "C",
                                        rlang::quo(formattr::n_percent(.n, .proportion)), NULL),
    tibble::tibble(`Patient characteristic` = c("C", "&emsp;C1", "&emsp;C2"),
                   value = c(NA, '7 (58%)', '8 (67%)'))
  )
  expect_equal(
    multiresponse_characteristics_table(df, "d", c(C1 = "c1", C2 = "c2"), "C",
                                        rlang::quo(formattr::n_percent(.n, .proportion)), "Total"),
    tibble::tibble(`Patient characteristic` = c("C", "&emsp;C1", "&emsp;C2"),
                   d0 = c(NA, '3 (50%)', '4 (67%)'),
                   d1 = c(NA, '4 (67%)', '4 (67%)'),
                   Total = c(NA, '7 (58%)', '8 (67%)'))
  )
})

test_that("create_descriptive_table works", {
  expect_equal(
    create_descriptive_table(df, c(A = "a"), c(B = "b"), list(C = c(C1 = "c1", C2 = "c2"))),
    tibble::tibble(
      `Patient characteristic` = c("A", "B", "&emsp;b1", "&emsp;b2", "&emsp;b3",
                                   "C", "&emsp;C1", "&emsp;C2"),
      value = c('6.5 (3.6)', NA, '4 (33%)', '5 (42%)', '3 (25%)', NA, '7 (58%)', '8 (67%)')
    )
  )
  expect_equal(
    create_descriptive_table(df, c(A = "a"), c(B = "b"), list(C = c(C1 = "c1", C2 = "c2")),
                             by = "d"),
    tibble::tibble(
      `Patient characteristic` = c("A", "B", "&emsp;b1", "&emsp;b2", "&emsp;b3",
                                   "C", "&emsp;C1", "&emsp;C2"),
      d0 = c('6.0 (3.7)', NA, '2 (33%)', '3 (50%)', '1 (17%)', NA, '3 (50%)', '4 (67%)'),
      d1 = c('7.0 (3.7)', NA, '2 (33%)', '2 (33%)', '2 (33%)', NA, '4 (67%)', '4 (67%)'),
      "Entire sample" = c('6.5 (3.6)', NA, '4 (33%)', '5 (42%)',
                          '3 (25%)', NA, '7 (58%)', '8 (67%)')
    )
  )
  expect_equal(
    create_descriptive_table(df, "a", "b", list(C = c(C1 = "c1", C2 = "c2"))),
    tibble::tibble(
      `Patient characteristic` = c("a", "b", "&emsp;b1", "&emsp;b2", "&emsp;b3",
                                   "C", "&emsp;C1", "&emsp;C2"),
      value = c('6.5 (3.6)', NA, '4 (33%)', '5 (42%)', '3 (25%)', NA, '7 (58%)', '8 (67%)')
    )
  )
})

test_that("make_header_row works", {
  expect_equal(make_header_row("A", df), tibble::tibble(a = "A", b = NA, c1 = NA, c2 = NA, d = NA))
})

test_that("create_descriptive_tables gives appropriate errors", {
  expect_error(create_descriptive_table(list()), class = "formattr_error_wrong_type")
  expect_error(create_descriptive_table(df, 1), class = "formattr_error_wrong_type")
  expect_error(create_descriptive_table(df, c(A = "a"), 1), class = "formattr_error_wrong_type")
  expect_error(create_descriptive_table(df, c(A = "a"), c(B = "b"), c(C = "c1")),
               class = "formattr_error_wrong_type")
  expect_error(
    create_descriptive_table(df, c(A = "a"), c(B = "b"), list(C = c(C1 = "c1", C2 = "c2")), 1),
    class = "formattr_error_wrong_type"
  )
  expect_error(
    create_descriptive_table(df, c(A = "a"), c(B = "b"), list(C = c(C1 = "c1", C2 = "c2")), by = 1),
    class = "formattr_error_wrong_type"
  )
  expect_error(
    create_descriptive_table(df, c(A = "a"), c(B = "b"), list(C = c(C1 = "c1", C2 = "c2")),
                             total = 1),
    class = "formattr_error_wrong_type"
  )
})
