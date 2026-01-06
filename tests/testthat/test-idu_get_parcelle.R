test_that("idu_get_parcelle() works offline with mocked dependencies", {
  fake_idus <- c("721870000A0001", "721870000A0002")

  fake_parts <- data.frame(
    idu = fake_idus,
    code_dep = "72",
    code_com = "187",
    prefix = "000",
    section = "0A",
    numero = c("0001", "0002"),
    insee = "72187"
  )

  fake_parcelles <- sf::st_sf(
    idu = fake_idus,
    geometry = sf::st_sfc(
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2))
    )
  )

  fake_feuilles <- sf::st_sf(
    id = c("F001", "F001"),
    geometry = sf::st_sfc(
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2))
    )
  )

  fake_lieudits <- sf::st_sf(
    nom = c("Lieu1", "Lieu2"),
    geometry = sf::st_sfc(
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2))
    )
  )

  fake_names <- data.frame(
    idu = fake_idus,
    code_reg = 52L,
    reg_name = "PAYS DE LA LOIRE",
    code_dep = "72",
    dep_name = "SARTHE",
    code_com = "72187",
    com_name = "MARIGNE LAILLE"
  )

  with_mocked_bindings(
    idu_split = function(idu) fake_parts,
    get_etalab = function(ids, layer = NULL, ...) {
      if (layer == "feuilles") {
        fake_feuilles
      } else if (layer == "lieux_dits") {
        fake_lieudits
      } else {
        fake_parcelles
      }
    },
    idu_get_cog = function(idu, ...) fake_names,
    st_join = sf::st_join,
    {
      res <- idu_get_parcelle(
        fake_idus,
        with_feuille = TRUE,
        with_lieudit = TRUE,
        with_cog = TRUE
      )

      expect_s3_class(res, "sf")
      expect_true(all(fake_idus %in% res$idu))
      expect_true("feuille" %in% names(res))
      expect_true("lieu_dit" %in% names(res))
      expect_true("com_name" %in% names(res))
    }
  )
})

test_that("idu_get_parcelle() works online with httptest2 mocks", {
  skip_if_not_installed("httptest2")

  httptest2::with_mock_dir("idu_get_parcelle", {
    # Test avec un IDU unique
    res_single <- idu_get_parcelle("721870000A0001")
    expect_s3_class(res_single, "sf")
    expect_true("idu" %in% names(res_single))

    # Test avec plusieurs IDUs
    res_multi <- idu_get_parcelle(c("721870000A0001", "721870000A0002"))
    expect_s3_class(res_multi, "sf")
    expect_true(all(c("idu") %in% names(res_multi)))

    # Test avec lieudit désactivé
    res_no_lieudit <- idu_get_parcelle(c("721870000A0001"), with_lieudit = FALSE)
    expect_s3_class(res_no_lieudit, "sf")
  })
})

# ONLINE TESTS ----
test_that("idu_get_parcelle() works online (smoke test)", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  idu <- "721870000A0001"

  res <- idu_get_parcelle(idu)

  expect_s3_class(res, "sf")
  expect_true(nrow(res) >= 1)
  expect_true("idu" %in% names(res))
})

test_that("idu_get_parcelle() online respects with_feuille flag", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  idu <- "721870000A0001"

  res_yes <- idu_get_parcelle(idu, with_feuille = TRUE)
  res_no  <- idu_get_parcelle(idu, with_feuille = FALSE)

  expect_s3_class(res_yes, "sf")
  expect_s3_class(res_no, "sf")

  expect_true("feuille" %in% names(res_yes))
  expect_false("feuille" %in% names(res_no))
})

test_that("idu_get_parcelle() online respects with_lieudit flag", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  idu <- "721870000A0001"

  res_yes <- idu_get_parcelle(idu, with_lieudit = TRUE)
  res_no  <- idu_get_parcelle(idu, with_lieudit = FALSE)

  expect_s3_class(res_yes, "sf")
  expect_s3_class(res_no, "sf")

  expect_true("lieu_dit" %in% names(res_yes))
  expect_false("lieu_dit" %in% names(res_no))
})

test_that("idu_get_parcelle() online respects with_cog flag", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  idu <- "721870000A0001"

  res_yes <- idu_get_parcelle(idu, with_cog = TRUE)
  res_no  <- idu_get_parcelle(idu, with_cog = FALSE)

  expect_s3_class(res_yes, "sf")
  expect_s3_class(res_no, "sf")

  cog_fields <- c("reg_name", "dep_name", "com_name")

  expect_true(all(cog_fields %in% names(res_yes)))
  expect_false(any(cog_fields %in% names(res_no)))
})

test_that("idu_get_parcelle() online supports multiple IDUs", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  idus <- c("721870000A0001", "721870000A0002")

  res <- idu_get_parcelle(idus)

  expect_s3_class(res, "sf")
  expect_true(all(idus %in% res$idu))
})

test_that("idu_get_parcelle() online handles missing lieudits gracefully", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  idu <- "72187000AB0001"

  expect_warning(
    res <- idu_get_parcelle(idu, with_lieudit = TRUE)
  )

  expect_s3_class(res, "sf")
})
