test_that("l'explicabilite reelle est bloquee sans modele", {
  result <- run_explainability_analysis(list(trained_models = NULL), list(), root = tempdir())
  expect_match(result$status, "NON EXPLICABLE")
  expect_true(all(result$outputs$PRODUITE == "NON"))
  expect_null(result$explanations)
})

test_that("le contrat refuse une fonction qui ne retourne pas des probabilites", {
  set.seed(151); x <- data.frame(x = rnorm(50)); y_num <- rbinom(50, 1, .4)
  model <- glm(y_num ~ x, family = binomial(), data = x)
  y <- factor(ifelse(y_num, "oui", "non"), levels = c("non", "oui"))
  expect_error(validate_explainability_inputs(model, x, y, function(object, newdata) rep(2, nrow(newdata))), "probabilite")
})

test_that("SHAP et permutation sont calculables sur une fixture technique", {
  set.seed(15); n <- 120L
  x <- data.frame(a = rnorm(n), b = rnorm(n)); y_num <- rbinom(n, 1, stats::plogis(x$a - .3 * x$b))
  model <- glm(y_num ~ ., family = binomial(), data = x)
  pred <- function(object, newdata) as.numeric(predict(object, newdata = newdata, type = "response"))
  shap <- compute_shap(model, x, y_num, pred, "fixture", simulations = 3L, observations = 3L)
  importance <- compute_global_importance(model, x, factor(ifelse(y_num, "oui", "non"), levels = c("non", "oui")),
    pred, "fixture", repetitions = 2L)
  expect_equal(nrow(shap$importance), ncol(x))
  expect_true(all(shap$importance$importance_shap_moyenne_absolue >= 0))
  expect_true(nrow(importance$vip_permutation) >= ncol(x))
  expect_true(nrow(importance$dalex_permutation) >= ncol(x))
})
