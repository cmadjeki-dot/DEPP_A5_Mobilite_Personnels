test_that("le rapport methodologique contient toutes les sections demandees", {
  path <- file.path(project_root, "reports", "rapport_methodologique.qmd")
  code <- readLines(path, encoding = "UTF-8", warn = FALSE)
  sections <- c("Contexte", "Problématique", "Sources", "Champ", "Population", "Qualité", "Préparation",
    "Variables", "Méthodes", "Descriptif", "Trajectoires", "Territoires", "Économétrie", "Survie",
    "Machine Learning", "Validation", "Explicabilité", "Limites", "Reproductibilité", "Conclusion")
  expect_true(all(paste0("## ", sections) %in% code))
})

test_that("le rapport documente la reproduction et les limites", {
  code <- paste(readLines(file.path(project_root, "reports", "rapport_methodologique.qmd"),
    encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_match(code, "renv::restore", fixed = TRUE)
  expect_match(code, "targets::tar_make", fixed = TRUE)
  expect_match(code, "testthat::test_dir", fixed = TRUE)
  expect_match(code, "Importance prédictive ≠ association statistique ≠ causalité", fixed = TRUE)
})
