test_that("idu_split correctly splits a standard mainland IDU", {
  idu <- "721870000A0001"
  res <- idu_split(idu)

  expect_s3_class(res, "data.frame")
  expect_equal(res$idu, idu)
  expect_equal(res$code_dep, "72")
  expect_equal(res$code_com, "187")
  expect_equal(res$prefix, "000")
  expect_equal(res$section, "0A")
  expect_equal(res$numero, "0001")
  expect_equal(res$insee, "72187")
})

test_that("idu_split correctly splits a DOM IDU", {
  idu <- "971020000A0001"
  res <- idu_split(idu)

  expect_equal(res$code_dep, "971")
  expect_equal(res$code_com, "02")
  expect_equal(res$insee, "97102")
})

test_that("idu_split handles vectorized input", {
  idus <- c("721870000A0001", "971020000A0001")
  res <- idu_split(idus)

  expect_equal(nrow(res), 2)
  expect_equal(res$idu, idus)
})

test_that("idu_split returns character columns", {
  res <- idu_split("721870000A0001")
  expect_type(res$code_dep, "character")
  expect_type(res$code_com, "character")
  expect_type(res$prefix, "character")
  expect_type(res$section, "character")
  expect_type(res$numero, "character")
  expect_type(res$insee, "character")
})

test_that("idu_split calls check_insee", {
  idu <- "721870000A0001"
  expect_silent(idu_split(idu))
})

test_that("idu_split works with vectorized IDUs", {
  idus <- c("721870000A0001", "731000000B0002")
  res <- idu_split(idus)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$idu, idus)
})
