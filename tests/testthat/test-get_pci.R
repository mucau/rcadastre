test_that("get_pci() works offline with mocked download and read functions using cache", {
  # Create a temporary cache
  cache <- file.path(tempdir(), "frcadastre")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache, recursive = TRUE, force = TRUE), add = TRUE)

  # Fake URLs to simulate downloads
  fake_urls <- c(
    "https://fake/edigeo-721870000A01.tar.bz2",
    "https://fake/edigeo-721870000C05.tar.bz2"
  )

  # Copy real extdata DXF files into temp cache
  fake_extract_path <- file.path(cache, "extdata")
  dir.create(fake_extract_path, recursive = TRUE, showWarnings = FALSE)

  file.copy(
    system.file("extdata/1870000A02.DXF", package = "frcadastre"),
    fake_extract_path,
    overwrite = TRUE
  )
  file.copy(
    system.file("extdata/1870000C05.DXF", package = "frcadastre"),
    fake_extract_path,
    overwrite = TRUE
  )

  # Mock functions
  with_mocked_bindings(
    get_pci_urls = function(x, millesime, format) fake_urls,
    download_archives = function(urls, destfiles, extract_dir, use_subdirs, verbose) {
      list(fake_extract_path)  # Return temp path, not committed files
    },
    read_dxf = function(path) {
      # Return a simple sf object for each DXF
      sf::st_sf(
        idu = basename(path),
        geometry = sf::st_sfc(sf::st_point(c(1,1)))
      )
    },
    read_edigeo = function(path) {
      # Return a list of sf objects
      list(
        sf::st_sf(
          idu = "fake_edigeo",
          geometry = sf::st_sfc(sf::st_point(c(2,2)))
        )
      )
    },
    {
      # Test DXF format
      pci_commune <- get_pci("72187", format = "dxf", extract_dir = seq_cache, verbose = FALSE)
      expect_s3_class(pci_commune, "sf")
      expect_true("idu" %in% names(pci_commune))

      # Test EDIGEO format
      pci_sheet <- get_pci("72181000AB01", format = "edigeo", extract_dir = seq_cache, verbose = FALSE)
      expect_true(is.list(pci_sheet))
      expect_true(all(sapply(pci_sheet, inherits, "sf")))
    }
  )
})

test_that("get_pci_urls() works offline with mocked detect_urls using cache", {
  # Temporary cache
  cache <- file.path(tempdir(), "frcadastre")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache, recursive = TRUE, force = TRUE), add = TRUE)

  fake_links <- c(
    "edigeo-721870000A01.tar.bz2",
    "edigeo-721870000B02.tar.bz2"
  )

  with_mocked_bindings(
    detect_urls = function(url, absolute = TRUE) file.path("https://fake", fake_links),
    get_data_millesimes = function(site) "latest",
    {
      urls <- get_pci_urls("72187", millesime = "latest", format = "edigeo")
      expect_true(all(grepl("^https://fake/", urls)))
      expect_true(all(grepl("\\.tar\\.bz2$", urls)))
      expect_equal(basename(urls), fake_links)
    }
  )
})
