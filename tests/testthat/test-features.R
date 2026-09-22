test_that("safe_ratio gère les dénominateurs nuls et manquants", {
  expect_equal(safe_ratio(c(1, 1, NA), c(2, 0, 2)), c(.5, NA, NA))
})

test_that("les indicateurs 1D sont agrégés, bornés et enrichis par UAI", {
  data <- data.frame(annee = 2024L, uai = "0010001A", secteur = "PUBLIC",
    code_departement = "01", code_academie = "10", code_region = "84",
    etp_d_enseignants_hommes_et_femmes = 10, etp_de_femmes_enseignantes = 7,
    etp_d_enseignants_de_moins_de_35_ans = 2, etp_d_enseignants_de_35_a_moins_de_50_ans = 5,
    etp_d_enseignants_de_50_ans_ou_plus = 3,
    etp_d_enseignants_ayant_une_anciennete_dans_l_etablissement_de_moins_de_2_ans = 1,
    etp_d_enseignants_ayant_une_anciennete_dans_l_etablissement_de_2_ans_a_moins_de_5_ans = 2,
    etp_d_enseignants_ayant_une_anciennete_dans_l_etablissement_de_5_ans_a_moins_de_8_ans = 3,
    etp_d_enseignants_ayant_une_anciennete_dans_l_etablissement_de_8_ans_ou_plus = 4)
  lookup <- data.frame(uai = "0010001A", dispositif_education_prioritaire = "REP")
  result <- build_personnel_1d_features(data, lookup)
  expect_equal(result$part_femmes, .7)
  expect_equal(result$proxy_stabilite_etablissement, .4)
  expect_true(result$education_prioritaire)
  expect_silent(validate_feature_table(result, c("annee", "uai"), "fixture"))
})

test_that("un UAI non apparié ne devient pas hors éducation prioritaire", {
  data <- data.frame(uai = c("A", "B"))
  lookup <- data.frame(uai = "A", dispositif_education_prioritaire = "HORS_EP")
  result <- append_education_priority(data, lookup)
  expect_false(result$education_prioritaire[[1]])
  expect_true(is.na(result$education_prioritaire[[2]]))
  expect_false(result$appariement_annuaire[[2]])
})

test_that("le dictionnaire documente les variables longitudinales indisponibles", {
  dictionary <- feature_dictionary()
  required <- c("NOM", "DÉFINITION", "FORMULE", "SOURCE", "JUSTIFICATION", "NA", "LIMITE")
  expect_true(all(required %in% names(dictionary)))
  unavailable <- c("age_individuel", "anciennete_individuelle", "changement_etablissement",
                   "changement_departement", "changement_academie", "changement_fonction", "mobilite_individuelle")
  expect_true(all(dictionary$FORMULE[dictionary$NOM %in% unavailable] == "Non construite"))
})

test_that("la validation refuse une part hors bornes", {
  data <- data.frame(annee = 2024L, uai = "A", part_test = 1.2)
  expect_error(validate_feature_table(data, c("annee", "uai"), "fixture"), "hors")
})
