test_that("composition_long distingue effectif, proportion et couverture", {
  data <- data.frame(annee = c(2024L, 2024L), total = c(10, 20), jeune = c(2, NA))
  result <- composition_long(data, "total", "jeune", "Moins de 35 ans", "Test", "Âge")
  expect_equal(result$etp, 2)
  expect_equal(result$etp_denominateur, 10)
  expect_equal(result$proportion, .2)
  expect_equal(result$couverture_etp, 1 / 3)
})

test_that("les métadonnées graphiques sont complètes", {
  labels <- descriptive_plot_labels("Titre", "Champ", "2024", "ETP", "Source", "Note")
  expect_identical(labels$title, "Titre")
  expect_match(labels$subtitle, "Champ.*Période.*Unité")
  expect_match(labels$caption, "Source.*Note de lecture")
})

test_that("la validation refuse proportions et figures invalides", {
  tables <- list(effectifs = data.frame(etp_enseignants = 1), age = data.frame(proportion = 1.2),
    anciennete = data.frame(proportion = .5), territoires = data.frame(annee = 2024, code_departement = "01"),
    grade = data.frame(disponibilite = "NON DISPONIBLE"))
  validation <- validate_descriptive_outputs(tables, figures = tempfile())
  expect_true(any(validation$STATUT == "ERREUR"))
})
