# MOCKED TESTS ----
test_that("get_pci_feuille() works offline with mocked detect_urls", {
  # Temporary cache (future-proof, même si la fonction n’en dépend pas encore)
  cache <- file.path(tempdir(), "frcadastre")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache, recursive = TRUE, force = TRUE), add = TRUE)

  fake_links <- c(
    "edigeo-721870000A01.tar.bz2",
    "edigeo-721870000B02.tar.bz2"
  )

  with_mocked_bindings(
    detect_urls = function(url, absolute = TRUE) fake_links,
    {
      feuilles_abs <- get_pci_feuille("72187", absolute = TRUE)
      feuilles_rel <- get_pci_feuille("72187", absolute = FALSE)

      expect_type(feuilles_abs, "character")
      expect_type(feuilles_rel, "character")

      expect_equal(feuilles_abs, fake_links)
      expect_equal(feuilles_rel, c("721870000A01", "721870000B02"))
    }
  )
})

test_that("get_pci_feuille() works with DXF format (mocked)", {
  fake_links <- c(
    "dxf-721870000A01.tar.bz2",
    "dxf-721870000B02.tar.bz2"
  )

  with_mocked_bindings(
    detect_urls = function(url, absolute = TRUE) fake_links,
    {
      feuilles <- get_pci_feuille("72187", format = "dxf", absolute = FALSE)
      expect_equal(feuilles, c("721870000A01", "721870000B02"))
    }
  )
})

test_that("get_pci_feuille() returns empty character vector if no sheets", {
  with_mocked_bindings(
    detect_urls = function(url, absolute = TRUE) character(0),
    {
      feuilles <- get_pci_feuille("72187")
      expect_type(feuilles, "character")
      expect_length(feuilles, 0)
    }
  )
})

test_that("get_pci_feuille() retrieves sheets for a real commune [httptest2]", {
  skip_if_not_installed("httptest2")

  httptest2::with_mock_dir("get_pci_feuille", {
    feuilles <- get_pci_feuille(
      "72187",
      millesime = "latest",
      format = "edigeo",
      absolute = FALSE
    )

    expect_type(feuilles, "character")
    expect_gt(length(feuilles), 0)
    expect_false(any(grepl("\\.tar\\.bz2$", feuilles)))
  })
})

# ONLINE TEST ----
test_that("get_pci_feuille() works online with real data", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  feuilles <- get_pci_feuille("72187", absolute = FALSE)

  expect_type(feuilles, "character")
  expect_gt(length(feuilles), 0)
  expect_false(any(grepl("\\.tar\\.bz2$", feuilles)))
})
