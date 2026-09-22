test_that("l'audit détecte doublons et incohérences sans modifier les données", {
  data <- data.frame(periode = c("2024", "2024"), id = c("A", "A"),
                     secteur = c("Public", "Inconnu"), mesure = c("1", "-2"), stringsAsFactors = FALSE)
  original <- data
  spec <- list(key = c("periode", "id"), time = "periode", expected_time = "2024",
               geo = character(), identifiers = character(), modalities = list(secteur = "Public"),
               nonnegative = "mesure", proportions = character(), relations = list())
  report <- audit_one_source(data, "fixture", spec)
  expect_identical(data, original)
  expect_true(all(c("INDICATEUR", "REGLE", "RESULTAT", "SEUIL", "STATUT", "ACTION") %in% names(report)))
  expect_true(any(report$CATEGORIE == "doublons" & report$STATUT == "ALERTE"))
  expect_true(any(report$CATEGORIE == "modalités" & report$STATUT == "ALERTE"))
  expect_true(any(report$CATEGORIE == "valeurs aberrantes" & report$STATUT == "ALERTE"))
})

test_that("une série à une période est explicitement non évaluable", {
  data <- data.frame(periode = "2024", id = "A", stringsAsFactors = FALSE)
  spec <- list(key = c("periode", "id"), time = "periode", expected_time = "2024",
               geo = character(), identifiers = character(), modalities = list(),
               nonnegative = character(), proportions = character(), relations = list())
  report <- audit_one_source(data, "fixture", spec)
  expect_true(any(report$CATEGORIE == "ruptures de série" & report$STATUT == "NON ÉVALUABLE"))
})
