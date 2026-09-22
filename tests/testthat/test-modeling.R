test_that("l'audit bloque le ML sur les donnees ouvertes", {
  panel <- readRDS(file.path(project_root, "data", "processed", "panel", "etablissement_annee_panel.rds"))
  audit <- audit_ml_feasibility(panel)
  expect_true(all(audit$STATUT == "BLOQUANT"))
})

test_that("les variables futures sont detectees", {
  x <- data.frame(id_personnel = 1:3, annee = 2020:2022, mobilite_t_plus_1 = c(0, 1, 0),
    age = 30:32, fonction_suivante = c("A", "B", "A"))
  expect_equal(audit_predictor_leakage(x), "fonction_suivante")
  expect_error(validate_ml_input(x), "Fuite")
})

test_that("la partition temporelle respecte strictement l'ordre", {
  x <- expand.grid(id_personnel = 1:4, annee = 2019:2022)
  x$mobilite_t_plus_1 <- rep(c(0, 1), length.out = nrow(x)); x$age <- 30
  split <- temporal_partition(x)
  expect_lt(max(split$train$annee), min(split$validation$annee))
  expect_lt(max(split$validation$annee), min(split$test$annee))
  folds <- temporal_resamples(rbind(split$train, split$validation))
  expect_s3_class(folds, "rset")
  workflows <- build_ml_workflows(split$train)
  expect_equal(sort(names(workflows)), sort(c("regression_logistique", "random_forest", "gradient_boosting")))
  final_split <- final_temporal_split(x)
  expect_lt(max(rsample::analysis(final_split)$annee), min(rsample::assessment(final_split)$annee))
})
