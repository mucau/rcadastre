test_that("idu_check returns logical TRUE vector when all IDUs are valid", {
  idus <- c("721870000A0001", "971020000B0002")
  result <- idu_check(idus)
  expect_true(all(result))
})

test_that("idu_check issues warning for single invalid IDU with verbose = TRUE", {
  expect_warning(
    result <- idu_check("invalidIDU", verbose = TRUE),
    "Invalid IDU"
  )
  expect_equal(result, FALSE)
})

test_that("idu_check issues warning listing multiple invalid IDUs", {
  idus <- c("721870000A0001", "invalid1", "invalid2")
  expect_warning(
    result <- idu_check(idus, verbose = TRUE),
    "Invalid IDU\\(s\\) detected: invalid1, invalid2"
  )
  expect_equal(result, c(TRUE, FALSE, FALSE))
})

test_that("idu_check detects NA and empty strings as invalid", {
  idus <- c("721870000A0001", NA, "")
  expect_warning(
    result <- idu_check(idus, verbose = TRUE),
    "Invalid IDU"
  )
  expect_equal(result, c(TRUE, FALSE, FALSE))
})

test_that("idu_check correctly identifies valid formats", {
  valid_idus <- c(
    "721870000A0001", # typical
    "971020000B0002", # department 97X
    "012340000Z9999"  # section letter Z
  )
  result <- idu_check(valid_idus)
  expect_true(all(result))
})

test_that("idu_check flags invalid formats correctly", {
  invalid_idus <- c(
    "72187",           # too short
    "72X870000A0001",  # invalid char in dep
    "721870000a0001",  # lowercase
    "721870000!0001",  # special char
    "721870000AA001",  # too short (13)
    "721870000AA00011" # too long (15)
  )
  expect_warning(
    result <- idu_check(invalid_idus, verbose = TRUE),
    "Invalid IDU"
  )
  expect_equal(result, rep(FALSE, length(invalid_idus)))
})

test_that("idu_check handles single valid IDU without warning", {
  expect_silent(result <- idu_check("721870000A0001"))
  expect_equal(result, TRUE)
})

test_that("idu_check returns logical vector with warning when some IDUs are invalid", {
  idus <- c("721870000A0001", "badIDU", "12345")
  expect_warning(
    result <- idu_check(idus, verbose = TRUE),
    "Invalid IDU"
  )
  expect_equal(result, c(TRUE, FALSE, FALSE))
})

test_that("idu_check warns but does not stop when all IDUs are invalid", {
  idus <- c("abc", "def", "ghi")
  expect_warning(
    result <- idu_check(idus, verbose = TRUE),
    "Invalid IDU"
  )
  expect_equal(result, rep(FALSE, 3))
})

test_that("idu_check emits only one warning even with multiple invalid IDUs", {
  idus <- c("bad1", "bad2", "bad3")
  expect_warning(
    result <- idu_check(idus, verbose = TRUE),
    regexp = "Invalid IDU\\(s\\) detected: bad1, bad2, bad3"
  )
  expect_equal(result, rep(FALSE, 3))
})

test_that("idu_check returns TRUE for valid and FALSE for invalid values mixed", {
  idus <- c("721870000A0001", "invalidIDU", "971020000B0002")
  expect_warning(
    result <- idu_check(idus, verbose = TRUE),
    "Invalid IDU"
  )
  expect_equal(result, c(TRUE, FALSE, TRUE))
})

test_that("idu_check works silently and returns all TRUE when all IDUs are valid", {
  idus <- c("721870000A0001", "971020000B0002")
  expect_silent(result <- idu_check(idus))
  expect_true(all(result))
})

test_that("idu_check returns FALSE for NA and empty strings but does not stop", {
  idus <- c(NA, "", "721870000A0001")
  expect_warning(
    result <- idu_check(idus, verbose = TRUE),
    "Invalid IDU"
  )
  expect_equal(result, c(FALSE, FALSE, TRUE))
})

test_that("idu_check handles empty input vector gracefully", {
  expect_warning(
    result <- idu_check(character(0), verbose = TRUE),
    "Input vector is empty"
  )
  expect_equal(result, logical(0))
})
