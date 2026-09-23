test_that("le README contient toutes les sections professionnelles demandees", {
  lines <- readLines(here::here("README.md"), warn = FALSE, encoding = "UTF-8")
  sections <- c(
    "Présentation", "Contexte", "Problématique", "Questions de recherche",
    "Données", "Méthodologie", "Architecture", "Pipeline", "Résultats",
    "Visualisations", "Dashboard", "Rapports", "Reproductibilité",
    "Installation", "Exécution", "Limites", "Compétences mobilisées"
  )
  expect_true(all(paste0("## ", sections) %in% lines))
})

test_that("le README ne surestime pas les resultats", {
  text <- paste(readLines(here::here("README.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(text, "aucun identifiant individuel stable", fixed = TRUE)
  expect_match(text, "sans identification causale", fixed = TRUE)
  expect_match(text, "ne prétend donc pas mesurer des trajectoires individuelles", fixed = TRUE)
  expect_match(text, "établissement × année", fixed = TRUE)
})

test_that("les visualisations et livrables lies existent", {
  paths <- c(
    "docs/assets/effectifs.png",
    "docs/assets/age_50_plus.png",
    "docs/assets/matrice_presence.png",
    "docs/reports/note_information.html",
    "docs/reports/rapport_methodologique.html",
    "dashboard/app.R"
  )
  paths <- here::here(paths)
  expect_true(all(file.exists(paths)))
  expect_true(all(file.info(paths)$size > 0))
})
