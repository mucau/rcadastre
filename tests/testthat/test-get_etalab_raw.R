# MOCKED TESTS ----
test_that("get_etalab_raw works offline with mocked dependencies", {
  fake_links <- c("https://example.org/pci-123-parcelles.json.gz")

  with_mocked_bindings(
    check_insee = function(commune, verbose = TRUE) rep(TRUE, length(commune)),
    get_insee_scale = function(x) rep("communes", length(x)),
    get_etalab_layernames = function(type) list(raw = "parcelles"),
    construct_data_url = function(type, commune, millesime) tempfile("base_"),
    detect_urls = function(base, absolute = TRUE) fake_links,
    download_archives = function(urls, destfiles, extract_dir, verbose = TRUE) list(TRUE),
    read_geojson = function(files) sf::st_sf(id = integer(0), geometry = sf::st_sfc()),
    {
      res <- get_etalab_raw("72187", "parcelles", verbose = FALSE)
      expect_s3_class(res, "sf")
      expect_named(res, c("id", "geometry"))
    }
  )
})

test_that("returns NULL if no URLs found", {
  with_mocked_bindings(
    check_insee = function(commune, verbose = TRUE) rep(TRUE, length(commune)),
    get_insee_scale = function(x) rep("communes", length(x)),
    get_etalab_layernames = function(type) list(raw = "parcelles"),
    construct_data_url = function(type, commune, millesime) tempfile("base_"),
    detect_urls = function(base, absolute = TRUE) character(0),
    {
      res <- get_etalab_raw("72187", "parcelles", verbose = FALSE)
      expect_null(res)
    }
  )
})

test_that("returns NULL if all downloads fail", {
  fake_links <- c("https://example.org/pci-123-parcelles.json.gz")

  with_mocked_bindings(
    check_insee = function(commune, verbose = TRUE) rep(TRUE, length(commune)),
    get_insee_scale = function(x) rep("communes", length(x)),
    get_etalab_layernames = function(type) list(raw = "parcelles"),
    construct_data_url = function(type, commune, millesime) tempfile("base_"),
    detect_urls = function(base, absolute = TRUE) fake_links,
    download_archives = function(urls, destfiles, extract_dir, verbose = TRUE) list(NULL),
    {
      res <- get_etalab_raw("72187", "parcelles", verbose = FALSE)
      expect_null(res)
    }
  )
})

# ONLINE TESTS  ----
test_that("get_etalab_raw works online with real server", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Single layer, single commune
  res <- get_etalab_raw("72187", "parcelle", verbose = FALSE)
  expect_true(inherits(res, "sf") || is.null(res))

  if (!is.null(res)) {
    expect_s3_class(res, "sf")
    expect_true(all(c("id", "geometry") %in% names(res)))
    expect_true(nrow(res) > 0)
  }

  # Invalid layer triggers error
  expect_error(get_etalab_raw("72187", "invalid_layer", verbose = FALSE))
})

test_that("get_etalab_raw handles multiple communes sequentially online", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  communes <- c("72187", "72181")
  layers <- "parcelle"

  results <- lapply(communes, function(c) {
    get_etalab_raw(c, layers, verbose = FALSE)
  })

  expect_true(all(sapply(results, function(x) is.null(x) || inherits(x, "sf"))))
})

test_that("invalid INSEE codes trigger error", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Code complètement invalide
  expect_error(
    get_etalab_raw("99999", "parcelles", verbose = FALSE),
    "Some INSEE codes are invalid"
  )

  # Mother commune code (Paris, Lyon, Marseille)
  expect_error(
    get_etalab_raw("75056", "parcelles", verbose = FALSE),
    "Some INSEE codes are invalid or correspond to mother communes"
  )
})

test_that("invalid layer triggers error", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  expect_error(
    get_etalab_raw("72187", "invalid_layer", verbose = FALSE),
    "Invalid processed layer: 'invalid_layer'"
  )
})

test_that("numeric INSEE codes work online", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  res <- get_etalab_raw(72187, "parcelle", verbose = FALSE)
  expect_true(is.null(res) || inherits(res, "sf"))

  if (!is.null(res)) {
    expect_s3_class(res, "sf")
    expect_true(all(c("id", "geometry") %in% names(res)))
    expect_true(nrow(res) > 0)
  }
})
