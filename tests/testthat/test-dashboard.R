test_that("le dashboard consomme uniquement les artefacts prepares", {
  app_file <- file.path(project_root, "dashboard", "app.R")
  code <- paste(readLines(app_file, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_false(grepl("tar_make\\s*\\(", code))
  expect_false(grepl("run_all_imports|run_all_cleaning|run_feature_engineering", code))
  expect_true(grepl("read_prepared", code, fixed = TRUE))
})

test_that("le dashboard utilise un cache Sass accessible en ecriture", {
  code <- paste(readLines(here::here("dashboard", "app.R"), warn = FALSE), collapse = "\n")
  expect_match(code, "tempdir()", fixed = TRUE)
  expect_match(code, "options(sass.cache = sass_cache)", fixed = TRUE)
})

test_that("les sept onglets demandes sont presents", {
  code <- paste(readLines(file.path(project_root, "dashboard", "app.R"), encoding = "UTF-8", warn = FALSE), collapse = "\n")
  tabs <- c("Vue générale", "Profils", "Mobilités", "Territoires", "Trajectoires", "Modélisation", "Méthodologie")
  expect_true(all(vapply(tabs, grepl, logical(1), x = code, fixed = TRUE)))
})

test_that("tous les artefacts requis existent", {
  validation <- validate_dashboard_artifacts(project_root)
  expect_true(all(validation$statut == "OK"))
})
