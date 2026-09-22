test_that("le nettoyage standardise sans modifier l'objet source", {
  raw <- data.frame(
    annee_de_la_rentree_scolaire = c("2024", "2024"),
    identifiant_de_l_etablissement = c(" 001abc01 ", "001abc01"),
    secteur = c("Public", "Privé sous contrat"),
    etp_total = c("12,5", "ss"), stringsAsFactors = FALSE
  )
  original <- raw
  cleaned <- clean_one_source(raw, "personnels_2d")
  expect_identical(raw, original)
  expect_identical(cleaned$annee, c(2024L, 2024L))
  expect_identical(cleaned$uai, c("001ABC01", "001ABC01"))
  expect_identical(cleaned$secteur, c("PUBLIC", "PRIVE_SOUS_CONTRAT"))
  expect_equal(cleaned$etp_total[[1]], 12.5)
  expect_true(is.na(cleaned$etp_total[[2]]))
  expect_identical(cleaned$etp_total_statut[[2]], "SECRET_STATISTIQUE")
})

test_that("les années scolaires, dates et codes géographiques sont typés", {
  raw <- data.frame(rentree_scolaire = "2022-2023", code_du_departement = "02A",
                    code_de_l_academie = "1", date_ouverture = "2020-09-01",
                    stringsAsFactors = FALSE)
  cleaned <- clean_one_source(raw, "fixture")
  expect_identical(cleaned$annee_debut, 2022L)
  expect_identical(cleaned$annee_fin, 2023L)
  expect_identical(cleaned$code_departement, "2A")
  expect_identical(cleaned$code_academie, "01")
  expect_s3_class(cleaned$date_ouverture, "Date")
})

test_that("un code RPI dispersé reste un identifiant et une modalité binaire inconnue est refusée", {
  raw <- data.frame(rpi_disperse = "07601A", ecole_maternelle = "1", stringsAsFactors = FALSE)
  cleaned <- clean_one_source(raw, "annuaire_education")
  expect_identical(cleaned$rpi_disperse, "07601A")
  expect_true(cleaned$ecole_maternelle)
  expect_error(as_logical_depp("INCONNU"), "non reconnue")
})

test_that("la table de transformation couvre les limites corps et grades", {
  dictionary <- cleaning_transformation_table()
  expect_true(all(c("VARIABLE BRUTE", "PROBLÈME", "TRANSFORMATION", "VARIABLE FINALE", "JUSTIFICATION") %in% names(dictionary)))
  expect_true(any(dictionary[["VARIABLE BRUTE"]] == "Corps individuel" & dictionary[["VARIABLE FINALE"]] == "non disponible"))
  expect_true(any(dictionary[["VARIABLE BRUTE"]] == "Grade individuel" & dictionary[["VARIABLE FINALE"]] == "non disponible"))
})
