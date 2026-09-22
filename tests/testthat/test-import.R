test_that("le manifeste decrit des sources publiques exploitables", {
  manifest <- read_source_manifest()
  required <- c("id", "producer", "dataset", "landing_url", "download_url", "vintage", "filename",
                "format", "encoding", "delimiter", "minimum_rows", "required_columns")
  expect_gte(length(manifest$sources), 1)
  expect_true(all(vapply(manifest$sources, function(x) all(required %in% names(x)), logical(1))))
  expect_true(all(vapply(manifest$sources, function(x) startsWith(x$landing_url, "https://"), logical(1))))
})

test_that("un CSV local est verifie et importe sans reseau", {
  root <- withr::local_tempdir()
  source <- list(id = "fixture", producer = "test", dataset = "fixture", landing_url = "https://example.org",
                 download_url = "https://example.org/file.csv", license = "test", vintage = "2026",
                 filename = "fixture.csv", format = "csv", encoding = "UTF-8", delimiter = ";",
                 minimum_rows = 2, required_columns = list("id", "valeur"))
  paths <- source_paths(source, root); fs::dir_create(dirname(paths$raw))
  writeLines(c("id;valeur", "001;A", "002;B"), paths$raw, useBytes = TRUE)
  checks <- validate_raw_csv(paths$raw, source); data <- import_raw_csv(paths$raw, source)
  expect_silent(validate_imported_data(data, source)); expect_identical(data$id, c("001", "002"))
  expect_match(checks$sha256, "^[0-9a-f]{64}$"); expect_equal(nrow(build_initial_dictionary(data, source$id)), 2)
})

test_that("le controle de schema refuse une colonne absente", {
  source <- list(required_columns = list("uai"), minimum_rows = 1)
  expect_error(validate_imported_data(data.frame(code = "x"), source), "uai")
})
