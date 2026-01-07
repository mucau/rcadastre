test_that("get_etalab_layernames returns correct names for raw", {
  res <- get_etalab_layernames("raw")
  expect_type(res, "list")
  expect_true("batiment" %in% res$raw)
  expect_equal(length(res$raw), 22)
})

test_that("get_etalab_layernames returns correct names for proc", {
  res <- get_etalab_layernames("proc")
  expect_type(res, "list")
  expect_true("parcelles" %in% res$proc)
  expect_equal(length(res$proc), 8) # 8 defined layers
})

test_that("get_etalab_layernames accepts multiple types", {
  res <- get_etalab_layernames(c("raw", "proc"))
  expect_named(res, c("raw", "proc"))
  expect_true("parcelles" %in% res$proc)
  expect_true("batiment" %in% res$raw)
})

test_that("get_etalab_layernames throws an error for invalid type", {
  expect_error(get_etalab_layernames("fake"), "Invalid type")
})
