test_that("le pipeline contient toutes les etapes industrialisees", {
  code <- paste(readLines(here::here("_targets.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expected <- c(
    "sources", "import_results", "quality_report", "cleaned_data", "feature_data",
    "panel_data", "descriptive_results", "trajectory_results", "territorial_results",
    "econometric_results", "survival_results", "modeling_results", "evaluation_results",
    "explainability_results", "figures", "tables", "reports", "notebooks",
    "config_files", "raw_files", "report_sources", "notebook_sources",
    "dashboard_source", "pipeline_manifest"
  )
  for (target in expected) expect_match(code, paste0("\\b", target, "\\b"))
})

test_that("les artefacts sont enregistres comme fichiers", {
  root <- here::here()
  manifest <- withr::with_dir(root, targets::tar_manifest(
    script = file.path(root, "_targets.R"),
    fields = c(name, format),
    callr_function = NULL
  ))
  file_targets <- manifest$name[manifest$format == "file"]
  expect_true(all(c(
    "config_files", "raw_files", "figures", "tables", "report_sources",
    "notebook_sources", "reports", "notebooks", "dashboard_source",
    "pipeline_manifest"
  ) %in% file_targets))
})

test_that("le manifeste compte les jeux et non les rubriques YAML", {
  sources <- list(manifest = read_source_manifest())
  path <- withr::local_tempfile(fileext = ".csv")
  write_pipeline_manifest(list(character(), TRUE), sources, path)
  result <- utils::read.csv2(path, stringsAsFactors = FALSE)
  expect_equal(as.integer(result$VERSION_OU_VALEUR[result$COMPOSANT == "sources"]),
    length(sources$manifest$sources))
})

test_that("le registre de fichiers refuse une sortie vide", {
  path <- withr::local_tempdir()
  expect_error(register_output_files(list(TRUE), path, "csv"), "Aucun artefact")
})
