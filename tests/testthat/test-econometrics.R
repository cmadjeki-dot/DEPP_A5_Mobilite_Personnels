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
