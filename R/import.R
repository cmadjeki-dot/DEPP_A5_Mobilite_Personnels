# Fonctions reproductibles d'acquisition et d'importation des donnees publiques.

read_source_manifest <- function(path = here::here("config", "data_sources.yml")) {
  manifest <- yaml::read_yaml(path)
  if (is.null(manifest$sources) || !length(manifest$sources)) stop("Le manifeste ne contient aucune source active.", call. = FALSE)
  manifest
}

source_paths <- function(source, root = here::here()) {
  stem <- tools::file_path_sans_ext(source$filename)
  list(raw = file.path(root, "data", "raw", source$id, source$vintage, source$filename),
       provenance = file.path(root, "data", "raw", source$id, source$vintage, paste0(stem, "_provenance.yml")),
       dictionary = file.path(root, "data", "interim", "dictionaries", paste0(source$id, "_dictionary.csv")),
       log = file.path(root, "outputs", "logs", paste0("import_", source$id, ".log")))
}

write_import_log <- function(path, level, message) {
  fs::dir_create(dirname(path))
  line <- sprintf("%s | %-5s | %s", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), level, message)
  cat(line, "\n", file = path, append = TRUE, sep = "")
  invisible(line)
}

sha256_file <- function(path) digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)

download_raw_source <- function(source, path) {
  fs::dir_create(dirname(path))
  if (file.exists(path)) return(list(path = path, downloaded = FALSE))
  temporary <- tempfile(pattern = paste0(source$id, "_"), fileext = ".download")
  on.exit(unlink(temporary), add = TRUE)
  status <- utils::download.file(source$download_url, temporary, mode = "wb", quiet = FALSE, method = "libcurl")
  if (!isTRUE(status == 0) || !file.exists(temporary) || file.info(temporary)$size <= 0) stop("Echec du telechargement de ", source$id, call. = FALSE)
  if (!file.rename(temporary, path)) stop("Impossible de finaliser le fichier raw.", call. = FALSE)
  list(path = path, downloaded = TRUE)
}

validate_raw_csv <- function(path, source) {
  if (!file.exists(path) || file.info(path)$size <= 0) stop("Fichier absent ou vide.", call. = FALSE)
  if (tolower(tools::file_ext(path)) != tolower(source$format)) stop("Extension inattendue.", call. = FALSE)
  connection <- file(path, open = "rb"); on.exit(close(connection), add = TRUE)
  probe <- readBin(connection, what = "raw", n = min(1048576, file.info(path)$size))
  if (any(probe == as.raw(0))) stop("Octet NUL detecte dans le CSV.", call. = FALSE)
  text <- rawToChar(probe)
  if (is.na(iconv(text, from = source$encoding, to = "UTF-8"))) stop("Encodage invalide.", call. = FALSE)
  header <- strsplit(sub("^\ufeff", "", strsplit(text, "\n", fixed = TRUE)[[1]][1]), source$delimiter, fixed = TRUE)[[1]]
  if (length(header) < 2) stop("Separateur ou en-tete invalide.", call. = FALSE)
  list(bytes = unname(file.info(path)$size), sha256 = sha256_file(path), columns_in_header = length(header))
}

import_raw_csv <- function(path, source) {
  data.table::fread(path, sep = source$delimiter, encoding = source$encoding, na.strings = c("", "NA"),
                    colClasses = "character", data.table = FALSE, showProgress = FALSE)
}

validate_imported_data <- function(data, source) {
  missing_columns <- setdiff(unlist(source$required_columns), names(data))
  if (length(missing_columns)) stop("Colonnes requises absentes: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  if (nrow(data) < source$minimum_rows) stop("Nombre de lignes inferieur au minimum attendu.", call. = FALSE)
  invisible(TRUE)
}

build_initial_dictionary <- function(data, source_id) {
  data.frame(source_id = source_id, variable = names(data),
             classe_importee = vapply(data, function(x) paste(class(x), collapse = "/"), character(1)),
             nombre_manquants = vapply(data, function(x) sum(is.na(x)), integer(1)),
             modalites_distinctes = vapply(data, data.table::uniqueN, integer(1), na.rm = TRUE),
             exemple = vapply(data, function(x) { y <- x[!is.na(x) & nzchar(x)]; if (length(y)) y[[1]] else NA_character_ }, character(1)),
             stringsAsFactors = FALSE)
}

write_utf8_csv <- function(data, path) {
  fs::dir_create(dirname(path)); data.table::fwrite(data, path, sep = ";", bom = TRUE, na = ""); path
}

write_provenance <- function(source, path, raw_checks, rows, columns, downloaded) {
  record <- list(source_id = source$id, organisme = source$producer, dataset = source$dataset,
                 page_officielle = source$landing_url, url_telechargement = source$download_url,
                 licence = source$license, millesime = source$vintage,
                 date_acquisition_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                 fichier = basename(path), octets = raw_checks$bytes, sha256 = raw_checks$sha256,
                 format = source$format, encodage_declare = source$encoding, separateur = source$delimiter,
                 lignes = rows, colonnes = columns, telecharge_pendant_cette_execution = downloaded,
                 regle_raw = "Fichier conserve sans modification; aucun ecrasement automatique.")
  yaml::write_yaml(record, file.path(dirname(path), paste0(tools::file_path_sans_ext(basename(path)), "_provenance.yml")), fileEncoding = "UTF-8")
  record
}

run_source_import <- function(source, root = here::here()) {
  paths <- source_paths(source, root); write_import_log(paths$log, "INFO", paste("Debut", source$id))
  tryCatch({
    acquisition <- download_raw_source(source, paths$raw); checks <- validate_raw_csv(paths$raw, source)
    data <- import_raw_csv(paths$raw, source); validate_imported_data(data, source)
    write_utf8_csv(build_initial_dictionary(data, source$id), paths$dictionary)
    write_provenance(source, paths$raw, checks, nrow(data), ncol(data), acquisition$downloaded)
    write_import_log(paths$log, "OK", sprintf("%s lignes; %s colonnes; sha256=%s", nrow(data), ncol(data), checks$sha256))
    data.frame(source_id = source$id, vintage = source$vintage, rows = nrow(data), columns = ncol(data),
               sha256 = checks$sha256, raw_path = paths$raw, status = "OK")
  }, error = function(error) { write_import_log(paths$log, "ERROR", conditionMessage(error)); stop(error) })
}

run_all_imports <- function(manifest = read_source_manifest(), root = here::here()) {
  results <- do.call(rbind, lapply(manifest$sources, run_source_import, root = root))
  write_utf8_csv(results, file.path(root, "data", "interim", "import_summary.csv")); results
}
