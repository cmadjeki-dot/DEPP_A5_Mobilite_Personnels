# Analyse territoriale descriptive des donnees publiques agregees.

territorial_levels <- function() {
  data.frame(
    NIVEAU = c("Departement", "Academie", "Region"), DISPONIBLE = TRUE,
    SOURCE = "DEPP, indicateurs de personnels du premier degre",
    UNITE_SOURCE = "Etablissement (UAI) agrege par territoire",
    LIMITE = "Ni donnees individuelles, ni effet causal du territoire",
    stringsAsFactors = FALSE)
}

territorial_ratio <- function(numerator, denominator) {
  observed <- !is.na(numerator) & !is.na(denominator)
  den <- sum(denominator[observed], na.rm = TRUE)
  num <- sum(numerator[observed], na.rm = TRUE)
  total <- sum(denominator, na.rm = TRUE)
  c(numerateur = num, denominateur = den,
    proportion = if (den > 0) num / den else NA_real_,
    couverture_etp = if (total > 0) den / total else NA_real_)
}

aggregate_personnel_territory <- function(data, level = c("departement", "academie", "region"),
                                          year = max(data$annee, na.rm = TRUE)) {
  level <- match.arg(level); code <- paste0("code_", level); name <- paste0("nom_", level)
  required <- c("annee", code, name, "uai", "etp_d_enseignants_hommes_et_femmes")
  if (!all(required %in% names(data))) stop("Variables territoriales manquantes.", call. = FALSE)
  dt <- data.table::as.data.table(data)[annee == year & !is.na(get(code))]
  total <- "etp_d_enseignants_hommes_et_femmes"
  indicators <- c(
    part_femmes = "etp_de_femmes_enseignantes",
    part_age_50_plus = "etp_d_enseignants_de_50_ans_ou_plus",
    part_anciennete_moins_2 = "etp_d_enseignants_ayant_une_anciennete_dans_l_etablissement_de_moins_de_2_ans",
    part_anciennete_8_plus = "etp_d_enseignants_ayant_une_anciennete_dans_l_etablissement_de_8_ans_ou_plus")
  missing <- setdiff(unname(indicators), names(dt))
  if (length(missing)) stop("Variables d'indicateur manquantes: ", paste(missing, collapse = ", "), call. = FALSE)
  base <- dt[, .(etablissements = data.table::uniqueN(uai), etp_total = sum(get(total), na.rm = TRUE)), by = c(code, name)]
  for (indicator in names(indicators)) {
    variable <- indicators[[indicator]]
    part <- dt[, as.list(territorial_ratio(get(variable), get(total))), by = c(code, name)]
    data.table::setnames(part, c("numerateur", "denominateur", "proportion", "couverture_etp"),
      paste0(indicator, c("_numerateur", "_denominateur", "", "_couverture")))
    part[, (name) := NULL]
    base <- merge(base, part, by = code, all.x = TRUE, sort = FALSE)
  }
  base[, `:=`(annee = as.integer(year), niveau = level)]
  data.table::setorderv(base, code)
  as.data.frame(base)
}

aggregate_priority_education <- function(features, year = max(features$annee, na.rm = TRUE)) {
  dt <- data.table::as.data.table(features)[annee == year & appariement_annuaire == TRUE & !is.na(code_departement)]
  result <- dt[, .(etp_apparies = sum(etp_enseignants, na.rm = TRUE),
    etp_education_prioritaire = sum(etp_enseignants * as.numeric(education_prioritaire), na.rm = TRUE),
    etablissements_apparies = .N), by = code_departement]
  result[, part_etp_education_prioritaire := data.table::fifelse(etp_apparies > 0, etp_education_prioritaire / etp_apparies, NA_real_)]
  result[, annee := as.integer(year)]
  as.data.frame(result)
}

read_department_geometries <- function(movement) {
  required <- c("code_departement", "nom_departement", "geom")
  if (!all(required %in% names(movement))) stop("Geometries departementales absentes.", call. = FALSE)
  dt <- unique(data.table::as.data.table(movement)[!is.na(geom), ..required], by = "code_departement")
  geometries <- gsub('""', '"', dt$geom, fixed = TRUE)
  valid <- vapply(geometries, jsonlite::validate, logical(1))
  if (!all(valid)) stop("GeoJSON invalide pour ", sum(!valid), " departement(s).", call. = FALSE)
  features <- vapply(seq_along(geometries), function(i) paste0(
    '{"type":"Feature","properties":{"row_id":', i, '},"geometry":', geometries[[i]], '}'), character(1))
  collection <- paste0('{"type":"FeatureCollection","features":[', paste(features, collapse = ","), ']}' )
  path <- tempfile(fileext = ".geojson"); on.exit(unlink(path), add = TRUE)
  writeLines(collection, path, useBytes = TRUE)
  shapes <- suppressWarnings(sf::st_read(path, quiet = TRUE, stringsAsFactors = FALSE))
  shapes <- shapes[order(shapes$row_id), ]
  shapes$code_departement <- dt$code_departement; shapes$nom_departement <- dt$nom_departement; shapes$row_id <- NULL
  shapes <- sf::st_make_valid(shapes); sf::st_crs(shapes) <- 4326
  shapes
}

territorial_map_labels <- function(title, indicator, denominator, field, year, source, note = NULL) {
  ggplot2::labs(title = title,
    subtitle = paste0("Indicateur : ", indicator, " | Denominateur : ", denominator,
      "\nChamp : ", field, " | Annee : ", year),
    caption = paste0("Source : ", source, if (!is.null(note)) paste0("\nNote de lecture : ", note) else ""),
    fill = "Proportion")
}

build_composition_effects <- function(departments) {
  specs <- data.frame(
    indicateur = c("Part des ETP ages de 50 ans ou plus", "Part des ETP avec moins de 2 ans dans l'etablissement",
      "Part des ETP avec 8 ans ou plus dans l'etablissement"),
    proportion = c("part_age_50_plus", "part_anciennete_moins_2", "part_anciennete_8_plus"),
    numerator = c("part_age_50_plus_numerateur", "part_anciennete_moins_2_numerateur", "part_anciennete_8_plus_numerateur"),
    denominator = c("part_age_50_plus_denominateur", "part_anciennete_moins_2_denominateur", "part_anciennete_8_plus_denominateur"),
    stringsAsFactors = FALSE)
  do.call(rbind, lapply(seq_len(nrow(specs)), function(i) {
    s <- specs[i, ]; p <- departments[[s$proportion]]; den <- sum(departments[[s$denominator]], na.rm = TRUE)
    weighted <- sum(departments[[s$numerator]], na.rm = TRUE) / den
    data.frame(indicateur = s$indicateur, moyenne_nationale_ponderee_etp = weighted,
      moyenne_simple_departements = mean(p, na.rm = TRUE), ecart_composition = weighted - mean(p, na.rm = TRUE),
      ecart_type_interdepartemental = stats::sd(p, na.rm = TRUE),
      q1 = as.numeric(stats::quantile(p, .25, na.rm = TRUE)), q3 = as.numeric(stats::quantile(p, .75, na.rm = TRUE)),
      departements_observes = sum(!is.na(p)))
  }))
}

build_territorial_maps <- function(mapped, root = here::here()) {
  output <- file.path(root, "outputs", "figures", "territorial"); fs::dir_create(output)
  theme_map <- ggplot2::theme_void(base_size = 10) + ggplot2::theme(
    plot.title.position = "plot", plot.caption.position = "plot", legend.position = "right")
  source <- "DEPP, indicateurs de personnels du premier degre; contours issus du mouvement 1D public 2019"
  plots <- list(
    age_50_plus = ggplot2::ggplot(mapped) + ggplot2::geom_sf(ggplot2::aes(fill = part_age_50_plus), color = "white", linewidth = .08) +
      ggplot2::scale_fill_viridis_c(labels = scales::label_percent(accuracy = 1), na.value = "grey85") +
      territorial_map_labels("Part des enseignants ages de 50 ans ou plus", "ETP ages de 50 ans ou plus",
        "ETP avec age renseigne", "Etablissements du premier degre, departements cartographies", 2025, source,
        "Le taux est calcule apres agregation des numerateurs et denominateurs.") + theme_map,
    anciennete_moins_2 = ggplot2::ggplot(mapped) + ggplot2::geom_sf(ggplot2::aes(fill = part_anciennete_moins_2), color = "white", linewidth = .08) +
      ggplot2::scale_fill_viridis_c(labels = scales::label_percent(accuracy = 1), na.value = "grey85") +
      territorial_map_labels("Part avec moins de deux ans d'anciennete dans l'etablissement", "ETP avec anciennete etablissement < 2 ans",
        "ETP avec anciennete etablissement renseignee", "Etablissements du premier degre, departements cartographies", 2025, source,
        "L'anciennete dans l'etablissement n'est pas l'anciennete de carriere ni une mobilite individuelle.") + theme_map,
    education_prioritaire = ggplot2::ggplot(mapped) + ggplot2::geom_sf(ggplot2::aes(fill = part_etp_education_prioritaire), color = "white", linewidth = .08) +
      ggplot2::scale_fill_viridis_c(labels = scales::label_percent(accuracy = 1), na.value = "grey85") +
      territorial_map_labels("Part des ETP en education prioritaire", "ETP des UAI REP ou REP+",
        "ETP des UAI appariees a l'annuaire", "Etablissements du premier degre apparies", 2025,
        "DEPP; Annuaire de l'education, snapshot 2026; contours mouvement 1D public 2019",
        "Le statut d'education prioritaire est un snapshot 2026 applique au millesime 2025.") + theme_map)
  vapply(names(plots), function(name) {
    path <- file.path(output, paste0(name, ".png")); ggplot2::ggsave(path, plots[[name]], width = 9, height = 7, dpi = 160, bg = "white"); path
  }, character(1))
}

validate_territorial_outputs <- function(departments, academies, regions, mapped, figures) {
  checks <- data.frame(
    INDICATEUR = c("Cle departement", "Cle academie", "Cle region", "Proportions bornees", "Geometries valides", "Couverture jointure cartographique", "Figures produites"),
    REGLE = c("Une ligne par code", "Une ligne par code", "Une ligne par code", "Valeurs entre 0 et 1", "Toutes les geometries valides", "Au moins 95 % des departements statistiques", "Tous les fichiers existent"),
    RESULTAT = c(sum(duplicated(departments$code_departement)), sum(duplicated(academies$code_academie)), sum(duplicated(regions$code_region)),
      sum(departments$part_age_50_plus < 0 | departments$part_age_50_plus > 1, na.rm = TRUE), sum(!sf::st_is_valid(mapped)),
      nrow(mapped) / nrow(departments), sum(file.exists(figures))),
    SEUIL = c("0", "0", "0", "0", "0", ">= 0.95", paste0("= ", length(figures))), stringsAsFactors = FALSE)
  checks$STATUT <- c(ifelse(checks$RESULTAT[1:5] == 0, "OK", "ERREUR"), ifelse(checks$RESULTAT[6] >= .95, "OK", "ERREUR"), ifelse(checks$RESULTAT[7] == length(figures), "OK", "ERREUR"))
  checks$ACTION <- ifelse(checks$STATUT == "OK", "Aucune", "Corriger avant diffusion")
  checks
}

run_territorial_analysis <- function(descriptive_results = NULL, root = here::here()) {
  clean <- file.path(root, "data", "interim", "clean")
  p1 <- readRDS(file.path(clean, "personnels_1d_clean.rds")); movement <- readRDS(file.path(clean, "mouvement_1d_clean.rds"))
  features <- readRDS(file.path(root, "data", "processed", "features", "personnels_1d_features.rds"))
  year <- max(p1$annee, na.rm = TRUE)
  departments <- aggregate_personnel_territory(p1, "departement", year)
  academies <- aggregate_personnel_territory(p1, "academie", year)
  regions <- aggregate_personnel_territory(p1, "region", year)
  priority <- aggregate_priority_education(features, year)
  shapes <- read_department_geometries(movement)
  # Le code est la cle de jointure; les libelles peuvent differer par accents ou traits d'union selon les sources.
  shapes$nom_departement <- NULL
  mapped <- merge(shapes, departments, by = "code_departement", all = FALSE)
  mapped <- merge(mapped, priority, by = "code_departement", all.x = TRUE)
  composition <- build_composition_effects(departments)
  figures <- build_territorial_maps(mapped, root)
  validation <- validate_territorial_outputs(departments, academies, regions, mapped, figures)
  if (any(validation$STATUT == "ERREUR")) stop("Validation territoriale echouee.", call. = FALSE)
  output <- file.path(root, "outputs", "tables", "territorial"); fs::dir_create(output)
  write_utf8_csv(territorial_levels(), file.path(output, "niveaux_geographiques.csv"))
  write_utf8_csv(departments, file.path(output, "indicateurs_departements_2025.csv"))
  write_utf8_csv(academies, file.path(output, "indicateurs_academies_2025.csv"))
  write_utf8_csv(regions, file.path(output, "indicateurs_regions_2025.csv"))
  write_utf8_csv(priority, file.path(output, "education_prioritaire_departements_2025.csv"))
  write_utf8_csv(composition, file.path(output, "effets_composition_territoriale.csv"))
  write_utf8_csv(validation, file.path(output, "validation_territoriale.csv"))
  list(levels = territorial_levels(), departments = departments, academies = academies, regions = regions,
    priority_education = priority, composition = composition, validation = validation, figures = figures)
}
