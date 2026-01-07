# MOCKED TESTS ----
test_that("returns sf for a valid processed layer", {
  with_mocked_bindings(
    check_insee = function(commune, verbose = TRUE) rep(TRUE, length(commune)),
    get_insee_scale = function(x) rep("communes", length(x)),
    get_etalab_layernames = function(type) list(proc = c("parcelles", "sections")),
    read_geojson = function(url) sf::st_sf(id = integer(0), geometry = sf::st_sfc()),
    {
      res <- get_etalab_proc("72187", "parcelles", verbose = FALSE)
      expect_s3_class(res, "sf")
      expect_named(res, c("id", "geometry"))
    }
  )
})

test_that("stops with error for invalid processed layer", {
  with_mocked_bindings(
    check_insee = function(commune, verbose = TRUE) rep(TRUE, length(commune)),
    get_insee_scale = function(x) rep("communes", length(x)),
    get_etalab_layernames = function(type) list(proc = c("parcelles", "sections")),
    {
      expect_error(
        get_etalab_proc("72187", "invalid_layer", verbose = FALSE),
        "Invalid processed layer: 'invalid_layer'"
      )
    }
  )
})

test_that("returns NULL if read_geojson fails", {
  with_mocked_bindings(
    check_insee = function(commune, verbose = TRUE) rep(TRUE, length(commune)),
    get_insee_scale = function(x) rep("communes", length(x)),
    get_etalab_layernames = function(type) list(proc = c("parcelles", "sections")),
    read_geojson = function(url) stop("download failed"),
    {
      res <- get_etalab_proc("72187", "parcelles", verbose = FALSE)
      expect_null(res)
    }
  )
})

test_that("prints message when verbose = TRUE", {
  with_mocked_bindings(
    check_insee = function(id, verbose = TRUE) rep(TRUE, length(id)),
    get_insee_scale = function(x) rep("communes", length(x)),
    get_etalab_layernames = function(type) list(proc = c("parcelles", "sections")),
    read_geojson = function(url) sf::st_sf(id = integer(0), geometry = sf::st_sfc()),
    {
      expect_message(
        get_etalab_proc("72187", "parcelles", verbose = TRUE),
        "Downloading parcelles for 72187"
      )
    }
  )
})

test_that("accepts numeric INSEE codes", {
  with_mocked_bindings(
    check_insee = function(id, verbose = TRUE) rep(TRUE, length(id)),
    get_insee_scale = function(x) rep("communes", length(x)),
    get_etalab_layernames = function(type) list(proc = "parcelles"),
    read_geojson = function(url) sf::st_sf(id = integer(0), geometry = sf::st_sfc()),
    {
      res <- get_etalab_proc(72187, "parcelles", verbose = FALSE)
      expect_s3_class(res, "sf")
    }
  )
})


# ONLINE TESTS ----
test_that("get_etalab_proc works online with real server", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Single layer, single commune
  res <- get_etalab_proc("72187", "parcelles", verbose = FALSE)
  expect_s3_class(res, "sf")
  expect_true(nrow(res) > 0)
  expect_true(all(c("id", "geometry") %in% names(res)))

  # Another single layer for a single commune
  res2 <- get_etalab_proc("72187", "sections", verbose = FALSE)
  expect_s3_class(res2, "sf")
  expect_true(nrow(res2) > 0)
  expect_true(all(c("id", "geometry") %in% names(res2)))

  # Invalid layer triggers error
  expect_error(get_etalab_proc("72187", "invalid_layer", verbose = FALSE))
})

test_that("get_etalab_proc handles multiple communes sequentially", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  communes <- c("72187", "72181")
  layers <- "parcelles"

  results <- lapply(communes, function(c) {
    get_etalab_proc(c, layers, verbose = FALSE)
  })

  expect_true(all(sapply(results, inherits, "sf")))
  expect_true(all(sapply(results, nrow) > 0))
})

test_that("invalid INSEE codes trigger error", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Code complètement invalide
  expect_error(
    get_etalab_proc("99999", "parcelles", verbose = FALSE),
    "Some INSEE codes are invalid"
  )

  # Mother commune code (Paris, Lyon, Marseille)
  expect_error(
    get_etalab_proc("75056", "parcelles", verbose = FALSE),
    "Some INSEE codes are invalid or correspond to mother communes"
  )
})

test_that("invalid layer triggers error", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  expect_error(
    get_etalab_proc("72187", "invalid_layer", verbose = FALSE),
    "Invalid processed layer: 'invalid_layer'"
  )
})

test_that("numeric INSEE codes work online", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  res <- get_etalab_proc(72187, "parcelles", verbose = FALSE)
  expect_s3_class(res, "sf")
  expect_true(nrow(res) > 0)
  expect_true(all(c("id", "geometry") %in% names(res)))
})
