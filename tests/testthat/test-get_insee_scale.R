test_that("get_insee_scale() returns correct scale", {
  # Single commune
  expect_equal(get_insee_scale("72187"), "communes")
  # Single department
  expect_equal(get_insee_scale("72"), "departements")
  # Mixed codes
  res <- get_insee_scale(c("72187", "72"))
  expect_equal(res, c("communes", "departements"))
})

test_that("get_insee_scale() fails for invalid codes", {
  expect_error(get_insee_scale("99999"), "Cannot determine scale")
  expect_error(get_insee_scale("ABC"), "Cannot determine scale")
})
