test_that("l'audit bloque l'analyse sur le panel ouvert", {
  panel <- readRDS(file.path(project_root, "data", "processed", "panel", "etablissement_annee_panel.rds"))
  audit <- audit_survival_feasibility(panel)
  expect_true(all(audit$STATUT == "BLOQUANT"))
})

test_that("le contrat refuse des donnees longitudinales incompletes", {
  expect_error(validate_survival_input(data.frame(id_personnel = 1)), "manquantes")
})

test_that("les fonctions de survie sont executables sur une fixture conforme", {
  set.seed(12); n <- 300L
  fixture <- data.frame(id_personnel = seq_len(n), date_origine = as.Date("2020-01-01"),
    date_fin = as.Date("2020-01-01") + sample(30:1500, n, replace = TRUE),
    evenement_mobilite = rbinom(n, 1, .45), groupe = sample(c("A", "B"), n, replace = TRUE),
    age_origine = sample(22:62, n, replace = TRUE), anciennete_origine = sample(0:30, n, replace = TRUE))
  fit <- fit_personnel_survival(fixture)
  expect_s3_class(fit$kaplan_meier, "survfit")
  expect_s3_class(fit$cox, "coxph")
  expect_true(all(c("estimate", "conf.low", "conf.high") %in% names(fit$hazard_ratios)))
  expect_s3_class(fit$proportional_hazards, "cox.zph")
})
