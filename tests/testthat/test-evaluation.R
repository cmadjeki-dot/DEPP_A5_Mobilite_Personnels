test_that("l'evaluation reelle est bloquee sans predictions", {
  result <- run_model_evaluation(list(trained_models = NULL, metrics = NULL), root = tempdir())
  expect_match(result$status, "NON EVALUABLE")
  expect_null(result$comparison)
})

test_that("toutes les metriques demandees sont calculees", {
  set.seed(14)
  fixture <- do.call(rbind, lapply(c("logistique", "foret"), function(model) do.call(rbind,
    lapply(c(2020L, 2021L), function(year) {
      truth <- factor(sample(c("non", "oui"), 100, replace = TRUE, prob = c(.75, .25)), levels = c("non", "oui"))
      signal <- ifelse(truth == "oui", .25, -.1) + ifelse(model == "foret", .05, 0)
      data.frame(modele = model, periode = year, ensemble = ifelse(year == 2021, "test", "validation"),
        verite = truth, .pred_oui = pmin(pmax(.25 + signal + rnorm(100, sd = .18), .001), .999))
    }))))
  out <- evaluate_model_predictions(fixture)
  expected <- c("roc_auc", "pr_auc", "precision", "recall", "f1", "sensibilite", "specificite", "accuracy")
  expect_true(all(expected %in% names(out$metrics)))
  expect_equal(sum(out$confusion$Freq), nrow(fixture))
  expect_true(all(c("brier", "ece") %in% names(out$calibration$summaries)))
  expect_equal(nrow(out$generalization), 2)
})

test_that("les performances d'entrainement sont refusees", {
  x <- data.frame(modele = "m", periode = 2020, ensemble = "train",
    verite = factor(c("non", "oui"), levels = c("non", "oui")), .pred_oui = c(.1, .9))
  expect_error(validate_evaluation_predictions(x), "entrainement")
})
