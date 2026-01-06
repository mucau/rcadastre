# MOCKED TEST ----
test_that("get_pci_urls() fails on invalid INSEE codes", {
  with_mocked_bindings(
    check_insee = function(commune, verbose = TRUE) FALSE,
    get_data_millesimes = function(site) "latest",
    {
      expect_error(get_pci_urls("72187"), "Some INSEE codes are invalid")
    }
  )
})

test_that("get_pci_urls() removes duplicates", {
  with_mocked_bindings(
    detect_urls = function(url, absolute = TRUE) c("https://fake/dup1.tar.bz2", "https://fake/dup1.tar.bz2"),
    get_data_millesimes = function(site) "latest",
    {
      urls <- get_pci_urls("72187")
      expect_length(urls, 1)
      expect_equal(urls, "https://fake/dup1.tar.bz2")
    }
  )
})

test_that("get_pci_urls() handles mixed commune and sheet codes", {
  with_mocked_bindings(
    detect_urls = function(url, absolute = TRUE) "https://fake/commune.tar.bz2",
    get_data_millesimes = function(site) "latest",
    {
      urls <- get_pci_urls(c("72187", "721870000A01"), millesime = "latest", format = "edigeo")
      expect_true(any(grepl("commune", urls)))
      expect_true(any(grepl("721870000A01\\.tar\\.bz2$", urls)))
    }
  )
})

test_that("get_pci_urls() works offline with cache and mocked detect_urls", {
  # Temporary cache
  cache <- file.path(tempdir(), "frcadastre")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache, recursive = TRUE, force = TRUE), add = TRUE)

  # Fake PCI tarballs
  fake_links <- c(
    "edigeo-721870000A01.tar.bz2",
    "edigeo-721870000B02.tar.bz2"
  )

  with_mocked_bindings(
    # Mock detect_urls to return fake URLs
    detect_urls = function(url, absolute = TRUE) file.path("https://fake", fake_links),
    # Mock cadastral version to always return "latest"
    get_data_millesimes = function(site) "latest",
    {
      # --- Test commune code ---
      urls_commune <- get_pci_urls("72187", millesime = "latest", format = "edigeo")
      expect_true(all(grepl("^https://fake/", urls_commune)))
      expect_true(all(grepl("\\.tar\\.bz2$", urls_commune)))
      expect_equal(basename(urls_commune), fake_links)

      # --- Test single sheet code ---
      url_sheet <- get_pci_urls("721870000A01", millesime = "latest", format = "edigeo")
      expect_length(url_sheet, 1)
      expect_true(grepl("721870000A01\\.tar\\.bz2$", url_sheet))
    }
  )
})

test_that("get_pci_urls() works with DXF format using mocked URLs", {
  cache <- file.path(tempdir(), "frcadastre")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache, recursive = TRUE, force = TRUE), add = TRUE)

  fake_links <- paste0("dxf-72187", c("0000A01", "0000B02"), ".tar.bz2")

  with_mocked_bindings(
    detect_urls = function(url, absolute = TRUE) file.path("https://fake", fake_links),
    get_data_millesimes = function(site) "latest",
    {
      urls <- get_pci_urls("72187", millesime = "latest", format = "dxf")
      expect_true(all(grepl("^https://fake/", urls)))
      expect_true(all(grepl("\\.tar\\.bz2$", urls)))
      expect_equal(basename(urls), fake_links)
    }
  )
})

test_that("get_pci_urls() handles multiple commune codes correctly", {
  cache <- file.path(tempdir(), "frcadastre")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache, recursive = TRUE, force = TRUE), add = TRUE)

  fake_links <- paste0("edigeo-72187", c("0000A01", "0000B02"), ".tar.bz2")

  with_mocked_bindings(
    detect_urls = function(url, absolute = TRUE) file.path("https://fake", fake_links),
    get_data_millesimes = function(site) "latest",
    {
      urls <- get_pci_urls(c("72187", "72187"), millesime = "latest", format = "edigeo")
      expect_true(all(grepl("^https://fake/", urls)))
      expect_true(all(grepl("\\.tar\\.bz2$", urls)))
      expect_equal(basename(urls), fake_links)
    }
  )
})

#ONLINE TEST ----
test_that("get_pci_urls() fails on invalid codes", {
  expect_error(get_pci_urls("123"), "Invalid code")
  expect_error(get_pci_urls(c("ABCDE", "1234")), "Invalid code")
})

test_that("get_pci_urls() works online with real commune code", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  urls <- get_pci_urls("72187", millesime = "latest", format = "edigeo")

  expect_true(all(grepl("\\.tar\\.bz2$", urls)))
  expect_true(all(grepl("^https://cadastre\\.data\\.gouv\\.fr/", urls)))
  expect_gt(length(urls), 0)
})

test_that("get_pci_urls() works with sheet code directly", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  url <- get_pci_urls("721870000A01", millesime = "latest", format = "edigeo")

  expect_length(url, 1)
  expect_true(grepl("721870000A01\\.tar\\.bz2$", url))
})
