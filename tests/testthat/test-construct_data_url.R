test_that("construct_data_url returns correct URLs for PCI", {
  url <- construct_data_url("pci", "72187", millesime = "latest", format = "edigeo")
  expect_match(url, "^https://cadastre.data.gouv.fr/data/dgfip-pci-vecteur/latest/edigeo/feuilles/72/72187$")
})

test_that("construct_data_url returns correct URLs for Etalab", {
  url <- construct_data_url("etalab", "72187")
  expect_match(url, "^https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/communes/72/72187$")
})

test_that("construct_data_url errors on invalid commune", {
  expect_error(construct_data_url("pci", "99999"),
               "Some INSEE codes are invalid or correspond to mother communes")
})

test_that("construct_data_url works for multiple communes", {
  urls <- construct_data_url("pci", c("72187", "72181"), millesime = "latest", format = "edigeo")
  expect_length(urls, 2)
})
