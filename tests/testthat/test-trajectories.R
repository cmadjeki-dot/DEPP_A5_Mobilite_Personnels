test_that("les séquences de présence ne deviennent pas des mobilités", {
  panel <- data.frame(annee = c(2024L, 2025L, 2024L, 2025L), uai = c("A", "A", "B", "C"),
    etp_enseignants = c(10, 12, 5, 7), education_prioritaire = c(FALSE, FALSE, TRUE, TRUE),
    dispositif_education_prioritaire = c("HORS_EP", "HORS_EP", "REP", "REP"))
  result <- build_establishment_presence_sequences(panel)
  expect_setequal(result$sequence_presence,
    c("OBSERVE → OBSERVE", "OBSERVE → ABSENT_DU_FICHIER", "ABSENT_DU_FICHIER → OBSERVE"))
  expect_false(any(c("mobilite", "stable", "sortie_personnel") %in% names(result)))
})

test_that("la matrice de présence conserve toutes les UAI", {
  sequences <- data.frame(`2024` = c("OBSERVE", "OBSERVE"),
    `2025` = c("OBSERVE", "ABSENT_DU_FICHIER"), check.names = FALSE)
  matrix <- build_presence_transition_matrix(sequences)
  expect_equal(sum(matrix$N), 2)
  expect_equal(sum(matrix$proportion_conditionnelle), 1)
})

test_that("les trajectoires professionnelles sont déclarées indisponibles", {
  feasibility <- professional_trajectory_feasibility()
  expect_true(all(feasibility$DISPONIBILITE == "NON DISPONIBLE"))
  expect_true(all(feasibility$DECISION == "NON CONSTRUIT"))
})
