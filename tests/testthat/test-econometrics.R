test_that("les donnees econometriques ont une issue binaire au niveau UAI", {
  panel <- readRDS(file.path(project_root, "data", "processed", "panel", "etablissement_annee_panel.rds"))
  data <- prepare_econometric_data(panel)
  expect_true(all(data$y_absence_2025 %in% 0:1))
  expect_equal(anyDuplicated(data$uai), 0)
  expect_gt(sum(data$y_absence_2025), 0)
})

test_that("les odds ratios et intervalles sont produits", {
  set.seed(11); x <- data.frame(y = rbinom(300, 1, .2), z = rnorm(300))
  out <- tidy_econometric_coefficients(glm(y ~ z, family = binomial(), data = x))
  expect_true(all(c("odds_ratio", "or_ic95_basse", "or_ic95_haute") %in% names(out)))
  expect_true(all(out$odds_ratio > 0))
})

test_that("le VIF detecte les fortes colinearites", {
  set.seed(11); x <- data.frame(a = rnorm(100)); x$b <- x$a + rnorm(100, sd = .01); x$c <- rnorm(100)
  out <- numeric_vif(x, c("a", "b", "c"))
  expect_true(out$vif[out$variable == "a"] > 5)
})

test_that("les erreurs standards groupees et l'influence sont produites", {
  set.seed(42)
  fixture <- data.frame(
    y_absence_2025 = rbinom(400, 1, .15),
    x = rnorm(400),
    code_departement = rep(sprintf("%02d", 1:20), each = 20),
    uai = sprintf("%08d", 1:400)
  )
  model <- glm(y_absence_2025 ~ x, family = binomial(), data = fixture)
  clustered <- clustered_logit_coefficients(model, fixture$code_departement)
  influence <- influence_diagnostics(model, fixture)
  sensitivity <- influence_sensitivity(model, fixture)
  expect_true(all(clustered$std.error_cluster_departement > 0))
  expect_equal(unique(clustered$groupes), 20)
  expect_equal(nrow(influence$top), 20)
  expect_equal(unique(sensitivity$reestimations), 20)
  expect_true(all(sensitivity$odds_ratio_min <= sensitivity$odds_ratio_max))
  expect_true(all(is.finite(sensitivity$odds_ratio_principal)))
})
