test_that("la publication exclut les donnees et secrets usuels", {
  ignore <- paste(readLines(here::here(".gitignore"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  required <- c(
    "data/raw/*", "data/external/*", "data/interim/*", "data/processed/*",
    ".Renviron", ".env", "*.pem", "*.key", "*.p12", "credentials*.json",
    "renv/library/", "_targets/"
  )
  expect_true(all(vapply(required, grepl, logical(1), x = ignore, fixed = TRUE)))

  tracked_data <- system2("git", c("ls-files", "data"), stdout = TRUE)
  expect_true(all(basename(tracked_data) == ".gitkeep"))
})

test_that("aucun secret ni chemin utilisateur ne figure dans les fichiers suivis", {
  tracked <- system2("git", "ls-files", stdout = TRUE)
  tracked <- tracked[file.exists(here::here(tracked))]
  text_extensions <- c("R", "Rmd", "qmd", "md", "txt", "yml", "yaml", "json", "csv", "html", "Rprofile")
  text_files <- tracked[tolower(tools::file_ext(tracked)) %in% tolower(text_extensions)]
  content <- vapply(text_files, function(path) {
    paste(readLines(here::here(path), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }, character(1))
  secret_pattern <- "AKIA[0-9A-Z]{16}|ghp_[A-Za-z0-9]{20,}|github_pat_[A-Za-z0-9_]{20,}|BEGIN (RSA |EC |OPENSSH )?PRIVATE KEY"
  expect_false(any(grepl(secret_pattern, content, perl = TRUE)))
  expect_false(any(grepl("C:[/\\\\]Users[/\\\\]", content, perl = TRUE)))
})

test_that("GitHub Pages publie uniquement la documentation statique", {
  workflow <- paste(readLines(here::here(".github", "workflows", "pages.yml"), warn = FALSE), collapse = "\n")
  expect_match(workflow, "path: docs", fixed = TRUE)
  expect_match(workflow, "pages: write", fixed = TRUE)
  expect_match(workflow, "id-token: write", fixed = TRUE)
  expect_false(grepl("data/", workflow, fixed = TRUE))
  expect_true(file.exists(here::here("docs", "index.html")))
  expect_true(file.exists(here::here("docs", ".nojekyll")))
})

test_that("la licence et les documents publics sont presents", {
  license <- paste(readLines(here::here("LICENSE.md"), warn = FALSE), collapse = "\n")
  expect_match(license, "MIT License", fixed = TRUE)
  expect_match(license, "Permission is hereby granted", fixed = TRUE)
  expect_true(all(file.exists(here::here(c(
    "README.md",
    "docs/reports/note_information.html",
    "docs/reports/rapport_methodologique.html"
  )))))
})
