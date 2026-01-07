# MOCKED TEST ----
test_that("get_etalab works offline with mocked dependencies - single layer", {

  fake_proc <- sf::st_sf(id = integer(0), geometry = sf::st_sfc())
  fake_raw  <- sf::st_sf(id = integer(0), geometry = sf::st_sfc())

  with_mocked_bindings(
    `get_etalab_layernames` = function(type) list(proc = "proc_layer", raw = "raw_layer"),
    `get_etalab_proc`       = function(id, layer, verbose = TRUE) fake_proc,
    `get_etalab_raw`        = function(id, layer, millesime = "latest", extract_dir = NULL, verbose = TRUE) fake_raw,
    {
      # Processed layer
      res1 <- get_etalab("72187", "proc_layer", verbose = FALSE)
      expect_s3_class(res1, "sf")

      # Raw layer
      res2 <- get_etalab("72187", "raw_layer", verbose = FALSE)
      expect_s3_class(res2, "sf")
    }
  )
})

test_that("get_etalab throws error for invalid layer", {
  with_mocked_bindings(
    `get_etalab_layernames` = function(type) list(proc = c("proc_layer"), raw = c("raw_layer")),
    {
      expect_error(
        get_etalab("72187", "invalid_layer"),
        "Invalid layer"
      )
    }
  )
})

# ONLINE TEST ----
test_that("get_etalab works online for real Etalab layers", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Test processed layer
  proc_layer <- "numvoie"  # exemple d’un layer processé disponible
  res_proc <- get_etalab("72187", proc_layer, verbose = FALSE)
  expect_true(inherits(res_proc, "sf"))

  # Test raw layer
  raw_layer <- "parcelles"
  res_raw <- get_etalab("72187", raw_layer, verbose = FALSE)
  expect_true(inherits(res_raw, "sf"))

  # Test multiple layers
  layers <- list(raw_layer, proc_layer)
  res_list <- lapply(layers, function(l) get_etalab("72187", l, verbose = FALSE))
  expect_true(all(sapply(res_list, inherits, "sf")))
})
