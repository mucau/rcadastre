test_that("get_data_url returns correct URL for PCI at feuilles scale", {
  url <- get_data_url(id = "72187000AB01", site = "pci")
  expect_identical(
    url,
    "https://cadastre.data.gouv.fr/data/dgfip-pci-vecteur/latest/edigeo/feuilles/72/72187"
  )
})

test_that("get_data_url returns correct URL for PCI at municipal scale", {
  url <- get_data_url(id = "72187", site = "pci")
  expect_identical(
    url,
    "https://cadastre.data.gouv.fr/data/dgfip-pci-vecteur/latest/edigeo/feuilles/72/72187"
  )
})

test_that("get_data_url returns correct URL for Etalab at commune scale", {
  url <- get_data_url(id = "72187", site = "etalab")
  expect_identical(
    url,
    "https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/communes/72/72187"
  )
})

test_that("get_data_url returns correct URL for Etalab at department scale", {
  url <- get_data_url(id = "72", site = "etalab")
  expect_identical(
    url,
    "https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/departements/72"
  )
})

test_that("get_data_url uses feuille scale for PCI sheet identifiers", {
  url <- get_data_url(id = "72187000AB01", site = "pci")
  expect_match(url, "/feuilles/")
})

test_that("get_data_url supports DXF format for PCI", {
  url <- get_data_url(id = "72187000AB01", site = "pci", format = "dxf")
  expect_match(url, "/dxf/")
})

test_that("format argument is ignored for Etalab", {
  url <- get_data_url(id = "72187", site = "etalab", format = "edigeo")
  expect_match(url, "/geojson/")
})

test_that("get_data_url errors on invalid INSEE identifier", {
  expect_error(
    get_data_url(id = "99999", site = "pci"),
    "Some INSEE codes are invalid or correspond to mother communes"
  )
})

test_that("get_data_url errors on PCI + department scale", {
  expect_error(
    get_data_url(id = "72", site = "pci"),
    "frcadastre doesn't handle the departmental scale for PCI."
  )
})

test_that("get_data_url errors on Etalab + feuille scale", {
  expect_error(
    get_data_url(id = "72187000AB01", site = "etalab"),
    "Sheet scale not availabe for Etalab"
  )
})

test_that("get_data_url works for multiple identifiers", {
  urls <- get_data_url(id = c("72187", "72181"), site = "etalab")
  expect_length(urls, 2)
})

test_that("get_data_url preserves vector order", {
  ids  <- c("72187", "72181")
  urls <- get_data_url(id = ids, site = "etalab")

  expect_true(all(mapply(grepl, ids, urls)))
})



