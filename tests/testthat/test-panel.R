test_that("la faisabilité refuse un panel sans identifiant personnel", {
  data <- data.frame(annee = 2024L, uai = "A")
  assessment <- assess_person_year_feasibility(data)
  expect_false(assessment$DISPONIBLE[assessment$CRITERE == "Unité personnel × année"])
})

test_that("le panel établissement ordonne et décale au sein de chaque UAI", {
  data <- data.frame(annee = c(2025L, 2024L, 2024L), uai = c("A", "A", "B"),
                     code_departement = c("01", "01", "02"), code_academie = c("10", "10", "09"),
                     etp_enseignants = c(12, 10, 5))
  panel <- build_establishment_panel(data)
  a <- panel[panel$uai == "A", ]
  expect_identical(a$annee, c(2024L, 2025L))
  expect_true(a$sequence_consecutive[[2]])
  expect_equal(a$variation_etp_enseignants[[2]], 2)
  expect_false(a$disparition_apres_observation[[1]])
  b <- panel[panel$uai == "B", ]
  expect_true(b$disparition_apres_observation)
})

test_that("une année manquante n'est pas traitée comme transition consécutive", {
  data <- data.frame(annee = c(2022L, 2024L), uai = c("A", "A"), code_departement = "01",
                     code_academie = "10", etp_enseignants = c(10, 12))
  panel <- build_establishment_panel(data)
  expect_equal(panel$annees_manquantes_avant[[2]], 1L)
  expect_false(panel$sequence_consecutive[[2]])
  expect_true(is.na(panel$variation_etp_enseignants[[2]]))
})

test_that("aucune transition individuelle n'est créée", {
  dictionary <- panel_transition_dictionary()
  individual <- dictionary$TRANSITION %in% c("stable", "mobilité établissement", "mobilité département",
                                              "mobilité académie", "changement fonction", "entrée", "sortie")
  expect_true(all(dictionary$STATUT[individual] == "NON CONSTRUCTIBLE"))
})
