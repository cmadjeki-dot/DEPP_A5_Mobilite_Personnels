test_that("qualite - les cles, types et doublons du panel sont valides", {
  panel <- readRDS(here::here("data", "processed", "panel", "etablissement_annee_panel.rds"))
  expect_equal(anyDuplicated(panel[c("annee", "uai")]), 0)
  expect_type(panel$annee, "integer")
  expect_type(panel$uai, "character")
  expect_type(panel$etp_enseignants, "double")
  expect_true(all(nchar(panel$uai) == 8L))
})

test_that("statistique - les indicateurs sont bornes et leurs sorties non vides", {
  panel <- readRDS(here::here("data", "processed", "panel", "etablissement_annee_panel.rds"))
  proportions <- grep("^part_|^proxy_", names(panel), value = TRUE)
  for (variable in proportions) {
    values <- panel[[variable]]
    expect_true(all(is.na(values) | (values >= 0 & values <= 1)), info = variable)
  }
  expected <- c(
    "outputs/tables/descriptive/effectifs.csv",
    "outputs/tables/trajectories/matrice_transition_presence.csv",
    "outputs/tables/territorial/indicateurs_departements_2025.csv",
    "outputs/tables/econometrics/coefficients_logit.csv",
    "docs/reports/rapport_methodologique.html",
    "docs/reports/note_information.html"
  )
  paths <- here::here(expected)
  expect_true(all(file.exists(paths)))
  expect_true(all(file.info(paths)$size > 0))
})

test_that("metier - nomenclatures et transitions restent conformes au champ", {
  panel <- readRDS(here::here("data", "processed", "panel", "etablissement_annee_panel.rds"))
  expect_setequal(unique(panel$secteur), c("PUBLIC", "PRIVE_SOUS_CONTRAT"))
  expect_true(all(grepl("^([0-9]{2}|2[AB]|9[0-9]{2})$", panel$code_departement)))
  expect_true(all(grepl("^[0-9]{2}$", panel$code_academie)))
  expect_type(panel$sequence_consecutive, "logical")
  expect_true(all(is.na(panel$variation_etp_enseignants[!panel$sequence_consecutive])))
  expect_false(any(c("id_personnel", "mobilite_individuelle", "sortie_personnel") %in% names(panel)))
})

test_that("logiciel - le verrou renv couvre les dependances detectees", {
  lock <- jsonlite::read_json(here::here("renv.lock"), simplifyVector = TRUE)
  dependencies <- renv::dependencies(here::here(), progress = FALSE)
  required <- unique(stats::na.omit(dependencies[["Package"]]))
  locked <- names(lock$Packages)
  base <- rownames(installed.packages(priority = "base"))
  expect_setequal(setdiff(required, c(locked, base, "DEPPA5MobilitePersonnels")), character())
  expect_identical(lock$R$Version, paste(R.version$major, R.version$minor, sep = "."))
  expect_true(all(vapply(lock$Packages, function(x) nzchar(x$Version), logical(1))))
})
