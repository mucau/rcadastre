test_that("check_insee() passes for valid codes", {
  # Valid commune
  expect_true(check_insee("72187"))
  # Valid department
  expect_true(check_insee("72"))
  # Mixed valid codes
  expect_equal(check_insee(c("72187", "72")), c(TRUE, TRUE))
})

test_that("check_insee() warns for mother communes", {
  mother_codes <- c(paris = "75056", lyon = "69123", marseille = "13055")

  for(code in mother_codes) {
    expect_warning(
      res <- check_insee(code),
      regexp = "mother commune"
    )
    # Should return FALSE for mother communes
    expect_false(res)
  }
})

test_that("check_insee() warns for invalid codes", {
  # Unknown codes
  expect_warning(
    res <- check_insee("99999"),
    regexp = "Invalid INSEE code"
  )
  expect_false(res)

  expect_warning(
    res <- check_insee("ABC"),
    regexp = "Invalid INSEE code"
  )
  expect_false(res)

  # Mixed valid + invalid
  expect_warning(
    res <- check_insee(c("72187", "99999")),
    regexp = "Invalid INSEE code"
  )
  expect_equal(res, c(TRUE, FALSE))
})
