# Fonctions de nettoyage et d'harmonisation.
# Les fonctions lisent les donnees raw et ecrivent uniquement dans data/interim.

clean_names_depp <- function(names) {
  value <- iconv(names, from = "UTF-8", to = "ASCII//TRANSLIT")
  value <- tolower(gsub("[^a-zA-Z0-9]+", "_", value))
  value <- gsub("(^_+|_+$)", "", value)
  make.unique(value, sep = "_")
}

trim_character_columns <- function(data) {
  data[] <- lapply(data, function(x) {
    if (!is.character(x)) return(x)
    value <- trimws(gsub("[[:space:]]+", " ", x))
    value[value == ""] <- NA_character_
    value
  })
  data
}

standard_name_map <- function() {
  c(
    identifiant_de_l_etablissement = "uai",
    nom_de_l_etablissement = "nom_etablissement",
    nom_de_l_etablissment = "nom_etablissement",
    code_du_departement = "code_departement",
    departement = "nom_departement",
    libelle_departement = "nom_departement",
    code_de_l_academie = "code_academie",
    academie = "nom_academie",
    libelle_academie = "nom_academie",
    code_region_insee = "code_region",
    region = "nom_region",
    reflibelle_region = "nom_region",
    annee_de_la_rentree_scolaire = "annee"
  )
}

rename_standard_columns <- function(data) {
  names(data) <- clean_names_depp(names(data))
  mapping <- standard_name_map()
  for (old in intersect(names(mapping), names(data))) {
    proposed <- unname(mapping[[old]])
    if (!proposed %in% names(data)) names(data)[names(data) == old] <- proposed
  }
  names(data) <- make.unique(names(data), sep = "_")
  data
}

normalise_department_code <- function(x) {
  value <- toupper(trimws(x))
  value[value %in% c("02A", "02B")] <- substring(value[value %in% c("02A", "02B")], 2)
  value
}

normalise_academy_code <- function(x) {
  value <- trimws(x)
  numeric <- grepl("^[0-9]{1,2}$", value)
  value[numeric] <- sprintf("%02d", as.integer(value[numeric]))
  value
}

normalise_sector <- function(x) {
  key <- toupper(iconv(trimws(x), from = "UTF-8", to = "ASCII//TRANSLIT"))
  result <- rep(NA_character_, length(key))
  result[key == "PUBLIC"] <- "PUBLIC"
  result[key == "PRIVE"] <- "PRIVE"
  result[key == "PRIVE SOUS CONTRAT"] <- "PRIVE_SOUS_CONTRAT"
  result[is.na(key)] <- NA_character_
  result[is.na(result) & !is.na(key)] <- paste0("NON_CLASSE__", key[is.na(result) & !is.na(key)])
  result
}

convert_missing_markers <- function(data) {
  for (variable in names(data)) {
    if (!is.character(data[[variable]])) next
    value <- data[[variable]]
    marked <- value %in% c("ss", "NS")
    if (any(marked, na.rm = TRUE)) {
      status <- rep("OBSERVE", length(value))
      status[is.na(value)] <- "MANQUANT_SOURCE"
      status[value == "ss"] <- "SECRET_STATISTIQUE"
      status[value == "NS"] <- "NON_SIGNIFICATIF"
      data[[paste0(variable, "_statut")]] <- status
      value[marked] <- NA_character_
      data[[variable]] <- value
    }
  }
  data
}

numeric_variables <- function(data) {
  candidates <- grep("^(etp_|ips($|_)|effectifs($|_)|ratio_|proportion_|anciennete_|latitude$|longitude$|coord[xy]_origine$)",
                     names(data), value = TRUE)
  setdiff(candidates, grep("_statut$", candidates, value = TRUE))
}

logical_variables_annuaire <- function() {
  c("ecole_maternelle", "ecole_elementaire", "voie_generale", "voie_technologique",
    "voie_professionnelle", "restauration", "hebergement", "ulis", "apprentissage",
    "segpa", "section_arts", "section_cinema", "section_theatre", "section_sport",
    "section_internationale", "section_europeenne", "lycee_agricole", "lycee_militaire",
    "lycee_des_metiers", "post_bac", "greta", "multi_uai", "rpi_concentre")
}

as_logical_depp <- function(x) {
  key <- toupper(iconv(trimws(x), from = "UTF-8", to = "ASCII//TRANSLIT"))
  allowed <- c("1", "O", "OUI", "TRUE", "0", "N", "NON", "FALSE")
  unknown <- !is.na(key) & !key %in% allowed
  if (any(unknown)) stop("Modalite binaire non reconnue: ", paste(unique(key[unknown]), collapse = ", "), call. = FALSE)
  result <- rep(NA, length(key))
  result[key %in% c("1", "O", "OUI", "TRUE")] <- TRUE
  result[key %in% c("0", "N", "NON", "FALSE")] <- FALSE
  result
}

validate_cleaned_source <- function(raw, cleaned, source_id) {
  if (nrow(raw) != nrow(cleaned)) stop("Le nettoyage a modifié le nombre de lignes: ", source_id, call. = FALSE)
  if (anyDuplicated(names(cleaned))) stop("Noms de variables dupliqués: ", source_id, call. = FALSE)
  character_columns <- cleaned[vapply(cleaned, is.character, logical(1))]
  residual_markers <- sum(vapply(character_columns, function(x) sum(x %in% c("ss", "NS"), na.rm = TRUE), integer(1)))
  if (residual_markers) stop("Marqueurs ss/NS non traités: ", source_id, call. = FALSE)
  category_columns <- intersect(c("secteur", "statut_public_prive"), names(cleaned))
  for (variable in category_columns) {
    unexpected <- setdiff(unique(cleaned[[variable]]), c("PUBLIC", "PRIVE", "PRIVE_SOUS_CONTRAT", NA_character_))
    if (length(unexpected)) stop("Catégorie non harmonisée dans ", variable, ": ", paste(unexpected, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

clean_one_source <- function(data, source_id) {
  result <- data
  result <- trim_character_columns(result)
  result <- rename_standard_columns(result)
  result <- convert_missing_markers(result)

  for (variable in intersect(numeric_variables(result), names(result))) {
    result[[variable]] <- as_number_quality(result[[variable]])
  }
  for (variable in intersect(c("annee", "num_ligne"), names(result))) {
    result[[variable]] <- suppressWarnings(as.integer(result[[variable]]))
  }
  if ("rentree_scolaire" %in% names(result)) {
    valid <- grepl("^[0-9]{4}-[0-9]{4}$", result$rentree_scolaire)
    result$annee_debut <- ifelse(valid, as.integer(substr(result$rentree_scolaire, 1, 4)), NA_integer_)
    result$annee_fin <- ifelse(valid, as.integer(substr(result$rentree_scolaire, 6, 9)), NA_integer_)
  }
  for (variable in intersect(c("date_ouverture", "date_maj_ligne"), names(result))) {
    result[[variable]] <- as.Date(result[[variable]], format = "%Y-%m-%d")
  }
  if ("secteur" %in% names(result)) result$secteur <- normalise_sector(result$secteur)
  if ("statut_public_prive" %in% names(result)) result$statut_public_prive <- normalise_sector(result$statut_public_prive)
  if ("code_departement" %in% names(result)) result$code_departement <- normalise_department_code(result$code_departement)
  if ("code_academie" %in% names(result)) result$code_academie <- normalise_academy_code(result$code_academie)
  if ("uai" %in% names(result)) result$uai <- toupper(trimws(result$uai))

  label_columns <- intersect(c("nom_etablissement", "nom_academie", "nom_departement", "nom_region",
                               "nom_commune", "libelle_nature", "nature_de_l_etablissement"), names(result))
  result[label_columns] <- lapply(result[label_columns], function(x) trimws(gsub("[[:space:]]+", " ", x)))

  if (source_id == "annuaire_education" && "uai" %in% names(result)) {
    occurrences <- ave(result$uai, result$uai, FUN = length)
    result$uai_occurrences <- as.integer(occurrences)
    result$uai_duplique <- occurrences > 1L
  }
  for (variable in intersect(logical_variables_annuaire(), names(result))) {
    result[[variable]] <- as_logical_depp(result[[variable]])
  }
  attr(result, "source_id") <- source_id
  attr(result, "cleaning_note") <- "Copie nettoyee; les donnees raw ne sont jamais modifiees."
  result
}

cleaning_transformation_table <- function() {
  data.frame(
    "VARIABLE BRUTE" = c(
      "Tous les noms", "Chaînes de caractères", "ss / NS", "identifiant_de_l_etablissement / uai",
      "nom_de_l_etablissement / nom_de_l_etablissment", "code_du_departement / code_departement",
      "departement / libelle_departement", "code_de_l_academie / code_academie",
      "academie / libelle_academie", "secteur / statut_public_prive",
      "annee_de_la_rentree_scolaire / annee", "rentree_scolaire", "date_ouverture / date_maj_ligne",
      "ETP, IPS, effectifs, ratios, pourcentages", "Indicateurs binaires de l'annuaire",
      "UAI multiples de l'annuaire", "Corps enseignants agrégés", "Corps individuel", "Grade individuel"
    ),
    "PROBLÈME" = c(
      "Casse, accents ou séparateurs potentiellement hétérogènes", "Espaces parasites et chaînes vides",
      "Secret statistique ou valeur non significative", "Noms hétérogènes; identifiant à préserver",
      "Noms hétérogènes et faute de frappe publiée", "Noms et formats corses hétérogènes",
      "Noms hétérogènes", "Noms hétérogènes et largeur variable", "Noms hétérogènes",
      "Casse, accents et niveaux différents selon les sources", "Nom hétérogène et type caractère",
      "Période scolaire non décomposée", "Dates importées comme texte", "Mesures importées comme texte",
      "Valeurs textuelles 0/1, oui/non", "75 UAI répétés, 82 occurrences excédentaires",
      "Colonnes d'ETP agrégées, pas des personnes", "Absent des données publiques", "Absent des données publiques"
    ),
    "TRANSFORMATION" = c(
      "snake_case ASCII et unicité des noms", "trim, espaces internes réduits, vide vers NA",
      "Valeur vers NA avec colonne _statut conservant le motif", "Renommage en uai, trim et majuscules",
      "Renommage en nom_etablissement", "Renommage et 02A/02B vers 2A/2B",
      "Renommage en nom_departement", "Renommage, format sur deux positions",
      "Renommage en nom_academie", "Recodage contrôlé PUBLIC/PRIVE/PRIVE_SOUS_CONTRAT",
      "Renommage en annee et conversion integer", "Conservation et extraction des années début/fin",
      "Conversion Date ISO", "Conversion numérique explicite", "Conversion logical explicite",
      "Aucune suppression; ajout du nombre d'occurrences et d'un drapeau", "Conversion numérique des ETP uniquement",
      "Aucune création", "Aucune création"
    ),
    "VARIABLE FINALE" = c(
      "noms normalisés", "chaînes normalisées", "variable + variable_statut", "uai",
      "nom_etablissement", "code_departement", "nom_departement", "code_academie", "nom_academie",
      "secteur / statut_public_prive", "annee", "rentree_scolaire + annee_debut + annee_fin",
      "date_ouverture / date_maj_ligne", "mêmes noms, type numeric", "mêmes noms, type logical",
      "uai_occurrences + uai_duplique", "etp_d_enseignants_*", "non disponible", "non disponible"
    ),
    "JUSTIFICATION" = c(
      "Noms stables pour la programmation", "Éviter les pseudo-modalités et expliciter les manquants",
      "Ne pas confondre secret/non-significativité avec zéro", "Clé d'établissement commune sans perte des zéros",
      "Appariement cohérent des établissements", "Nomenclature géographique harmonisée",
      "Libellé départemental commun", "Code académique comparable", "Libellé académique commun",
      "Catégories comparables sans fusion abusive du privé", "Calculs temporels reproductibles",
      "Conserver le millésime original et faciliter les calculs", "Contrôles chronologiques fiables",
      "Calculs statistiques sans conversion implicite", "Sémantique binaire explicite",
      "Tracer l'anomalie sans dédoublonnage silencieux", "Respecter la granularité agrégée publiée",
      "Ne pas inventer une information individuelle", "Ne pas inventer une information individuelle"
    ), stringsAsFactors = FALSE, check.names = FALSE
  )
}

run_all_cleaning <- function(manifest = read_source_manifest(), quality_report = NULL,
                             root = here::here()) {
  output_directory <- file.path(root, "data", "interim", "clean")
  fs::dir_create(output_directory)
  summaries <- vector("list", length(manifest$sources)); paths <- character(length(manifest$sources))
  for (i in seq_along(manifest$sources)) {
    source <- manifest$sources[[i]]
    raw_path <- source_paths(source, root)$raw
    raw_hash_before <- sha256_file(raw_path)
    raw_data <- import_raw_csv(raw_path, source)
    cleaned <- clean_one_source(raw_data, source$id)
    validate_cleaned_source(raw_data, cleaned, source$id)
    path <- file.path(output_directory, paste0(source$id, "_clean.rds"))
    saveRDS(cleaned, path, version = 3)
    if (!identical(raw_hash_before, sha256_file(raw_path))) stop("Le fichier raw a été modifié: ", source$id, call. = FALSE)
    paths[[i]] <- path
    summaries[[i]] <- data.frame(source_id = source$id, rows = nrow(cleaned), columns = ncol(cleaned),
                                  output = path, raw_sha256 = raw_hash_before)
  }
  transformations <- cleaning_transformation_table()
  write_utf8_csv(transformations, file.path(output_directory, "table_transformations.csv"))
  summary <- do.call(rbind, summaries)
  write_utf8_csv(summary, file.path(output_directory, "cleaning_summary.csv"))
  list(files = paths, summary = summary, transformations = transformations)
}
