test_that("la note contient le titre et toutes les sections demandees", {
  path <- file.path(project_root, "reports", "note_information.qmd")
  code <- readLines(path, encoding = "UTF-8", warn = FALSE)
  full <- paste(code, collapse = "\n")
  expect_match(full, "Parcours et mobilité des personnels enseignants", fixed = TRUE)
  sections <- c("L'ESSENTIEL", "Population étudiée", "Principaux résultats", "Évolutions", "Profils", "Mobilités",
    "Territoires", "Analyse toutes choses égales par ailleurs", "Méthodologie", "Sources", "Définitions", "Limites")
  expect_true(all(vapply(sections, function(section) grepl(paste0("## ", section), full, fixed = TRUE), logical(1))))
})

test_that("la note distingue observation modele et causalite", {
  code <- paste(readLines(file.path(project_root, "reports", "note_information.qmd"),
    encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_match(code, "Observation", fixed = TRUE)
  expect_match(code, "Régression logistique", fixed = TRUE)
  expect_match(code, "Aucun résultat ne mesure un effet causal", fixed = TRUE)
  expect_match(code, "ne permettent pas de mesurer une trajectoire professionnelle", fixed = TRUE)
})
