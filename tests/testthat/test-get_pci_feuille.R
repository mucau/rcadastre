# MOCKED TESTS ----
test_that("get_pci_feuille() works offline with cache and mocked detect_urls", {
  # Create a temporary cache
  cache <- file.path(tempdir(), "frcadastre_cache")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache, recursive = TRUE, force = TRUE), add = TRUE)

  # Mocked links
  fake_links <- c(
    "edigeo-721870000A01.tar.bz2",
    "edigeo-721870000B02.tar.bz2"
  )

  # Fake extraction directory
  fake_extract_path <- file.path(cache, "extdata")
  dir.create(fake_extract_path, recursive = TRUE, showWarnings = FALSE)

  # Mock internal functions
  with_mocked_bindings(
    detect_urls = function(url, absolute = TRUE) fake_links,
    get_data_url = function(commune, site, millesime, format) {
      paste0("https://fake/", commune, "-", format, ".tar.bz2")
    },
    download_archives = function(urls, destfiles, extract_dir, use_subdirs, verbose) {
      # Simulate extraction by returning path to the cache
      list(fake_extract_path)
    },
    {
      # Test with absolute = TRUE
      feuilles_abs <- get_pci_feuille("72187", absolute = TRUE)
      expect_type(feuilles_abs, "character")
      expect_equal(feuilles_abs, fake_links)

      # Test with absolute = FALSE
      feuilles_rel <- get_pci_feuille("72187", absolute = FALSE)
      expect_type(feuilles_rel, "character")
      expect_equal(feuilles_rel, c("721870000A01", "721870000B02"))

      # Test DXF format
      fake_links_dxf <- c("dxf-721870000A01.tar.bz2", "dxf-721870000B02.tar.bz2")
      detect_urls <- function(url, absolute = TRUE) fake_links_dxf

      feuilles_dxf <- get_pci_feuille("72187", format = "dxf", absolute = FALSE)
      expect_equal(feuilles_dxf, c("721870000A01", "721870000B02"))
    }
  )
})

# ONLINE TEST ----
test_that("get_pci_feuille() works online with real EDIGEO data", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  feuilles <- get_pci_feuille("72187", absolute = FALSE)

  expect_type(feuilles, "character")
  expect_gt(length(feuilles), 0)
  expect_false(any(grepl("\\.tar\\.bz2$", feuilles)))
})

test_that("get_pci_feuille() works online with real DXF data", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  feuilles <- get_pci_feuille("72187", format = "dxf", absolute = FALSE)

  expect_type(feuilles, "character")
  expect_gt(length(feuilles), 0)
  expect_false(any(grepl("\\.tar\\.bz2$", feuilles)))
})
