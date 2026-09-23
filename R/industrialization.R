# Fonctions d'industrialisation du pipeline targets.

register_output_files <- function(dependencies, directory, extensions, root = here::here()) {
  invisible(dependencies)
  path <- file.path(root, directory)
  files <- list.files(path, recursive = TRUE, full.names = TRUE)
  files <- files[tolower(tools::file_ext(files)) %in% tolower(extensions)]
  files <- sort(normalizePath(files, winslash = "/", mustWork = TRUE))
  if (!length(files)) stop("Aucun artefact trouve dans ", path, ".", call. = FALSE)
  files
}

render_pipeline_reports <- function(dependencies, reports, root = here::here()) {
  invisible(dependencies)
  sources <- file.path(root, reports)
  missing <- sources[!file.exists(sources)]
  if (length(missing)) stop("Rapport source absent : ", paste(missing, collapse = ", "), call. = FALSE)

  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(root)
  for (source in reports) {
    output <- system2("quarto", c("render", shQuote(source)), stdout = TRUE, stderr = TRUE)
    exit_status <- attr(output, "status")
    if (!is.null(exit_status) && exit_status != 0L) {
      stop("Echec du rendu Quarto de ", source, ":\n", paste(output, collapse = "\n"), call. = FALSE)
    }
  }

  outputs <- file.path(root, "docs", sub("\\.qmd$", ".html", reports))
  missing_outputs <- outputs[!file.exists(outputs)]
  if (length(missing_outputs)) {
    stop("Sortie Quarto absente : ", paste(missing_outputs, collapse = ", "), call. = FALSE)
  }
  normalizePath(outputs, winslash = "/", mustWork = TRUE)
}

write_pipeline_manifest <- function(dependencies, sources, path, root = here::here()) {
  invisible(dependencies)
  output <- file.path(root, path)
  fs::dir_create(dirname(output))
  manifest <- data.frame(
    COMPOSANT = c("R", "targets", "Quarto", "sources", "rapports"),
    VERSION_OU_VALEUR = c(
      R.version.string,
      as.character(utils::packageVersion("targets")),
      quarto_version(),
      as.character(length(sources$manifest)),
      as.character(length(dependencies[[1]]))
    ),
    STATUT = "OK",
    stringsAsFactors = FALSE
  )
  write_utf8_csv(manifest, output)
  normalizePath(output, winslash = "/", mustWork = TRUE)
}

quarto_version <- function() {
  value <- system2("quarto", "--version", stdout = TRUE, stderr = TRUE)
  status <- attr(value, "status")
  if (!is.null(status) && status != 0L) return("INDISPONIBLE")
  trimws(value[[1]])
}
