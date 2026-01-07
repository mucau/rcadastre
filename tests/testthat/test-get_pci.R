# MOCKED TEST ----
test_that("get_pci() works offline with mocked download and read functions", {
  cache <- file.path(tempdir(), "frcadastre")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache, recursive = TRUE, force = TRUE), add = TRUE)

  fake_urls <- c(
    "https://fake/edigeo-721870000A01.tar.bz2",
    "https://fake/edigeo-721870000C05.tar.bz2"
  )

  fake_extract_path <- file.path(cache, "extdata")
  dir.create(fake_extract_path, recursive = TRUE, showWarnings = FALSE)

  # Mock functions
  with_mocked_bindings(
    get_data_millesimes = function(site) "latest",
    detect_urls = function(urls, absolute = TRUE) fake_urls,
    get_data_url = function(codes, site) paste0("https://fake/", codes, ".tar.bz2"),
    download_archives = function(urls, destfiles, extract_dir, use_subdirs, verbose) {
      list(fake_extract_path)
    },
    read_dxf = function(path) {
      sf::st_sf(
        idu = basename(path),
        geometry = sf::st_sfc(sf::st_point(c(1,1)))
      )
    },
    read_edigeo = function(path) {
      # Mocked read_edigeo accepts any path
      list(
        sf::st_sf(
          idu = "fake_edigeo",
          geometry = sf::st_sfc(sf::st_point(c(2,2)))
        )
      )
    },
    {
      # Test DXF
      pci_dxf <- get_pci("72187", format = "dxf", extract_dir = cache, verbose = FALSE)
      expect_s3_class(pci_dxf, "sf")
      expect_true("idu" %in% names(pci_dxf))

      # Test EDIGEO
      pci_edigeo <- get_pci("72181000AB01", format = "edigeo", extract_dir = cache, verbose = FALSE)
      expect_true(is.list(pci_edigeo))
      expect_true(all(sapply(pci_edigeo, inherits, "sf")))
    }
  )
})

# ONLINE TEST ----
test_that("get_pci() works online for a real IDU for EDIGEO", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Attempt to download EDIGEO data
  pci_data <- get_pci("721870000A01", format = "edigeo", verbose = FALSE)

  # Check that result is a list of sf objects
  expect_type(pci_data, "list")
  expect_true(all(sapply(pci_data, inherits, "sf")))

  # Check that at least one sf object has features
  expect_true(any(sapply(pci_data, function(x) nrow(x) > 0)))
})

test_that("get_pci() works online for a real IDU for DXF", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Attempt to download EDIGEO data
  pci_data <- get_pci("721870000A01", format = "dxf", verbose = FALSE)

  expect_s3_class(pci_data, "sf")
  expect_true(nrow(pci_data) > 0)
})
