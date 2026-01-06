test_that("idu_get_cog returns requested location columns for both", {
  idus <- c("721870000A0001", "721870000A0002")
  res <- idu_get_cog(idus, loc = c("reg", "dep", "com"), cog_field = "NCC")

  expect_true(all(c("idu", "reg_name", "dep_name", "com_name") %in% names(res)))
  expect_equal(nrow(res), length(idus))
})

test_that("idu_get_cog returns only region names when loc='reg'", {
  idus <- c("721870000A0001")
  res <- idu_get_cog(idus, loc = "reg", cog_field = "NCC")

  expect_true(all(c("idu", "reg_name") %in% names(res)))
  expect_false("dep_name" %in% names(res))
  expect_false("com_name" %in% names(res))
})

test_that("idu_get_cog returns only department names when loc='dep'", {
  idus <- c("721870000A0001")
  res <- idu_get_cog(idus, loc = "dep", cog_field = "NCC")

  expect_true(all(c("idu", "dep_name") %in% names(res)))
  expect_false("reg_name" %in% names(res))
  expect_false("com_name" %in% names(res))
})

test_that("idu_get_cog returns only commune names when loc='com'", {
  idus <- c("721870000A0001")
  res <- idu_get_cog(idus, loc = "com", cog_field = "NCC")

  expect_true(all(c("idu", "com_name") %in% names(res)))
  expect_false("reg_name" %in% names(res))
  expect_false("dep_name" %in% names(res))
})

test_that("idu_get_cog stops for invalid IDUs", {
  err <- expect_error(idu_get_cog(c("invalidIDU")))
  expect_match(err$message, "invalidIDU")
})
