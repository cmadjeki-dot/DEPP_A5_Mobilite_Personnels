# Fonctions d'audit et de controle de la qualite, sans modification des donnees.

quality_result <- function(source, category, rule, result, threshold, status, action) {
  data.frame(SOURCE = source, CATEGORIE = category, INDICATEUR = category,
             REGLE = rule, RESULTAT = as.character(result), SEUIL = threshold,
             STATUT = status, ACTION = action, stringsAsFactors = FALSE)
}

as_number_quality <- function(x) suppressWarnings(as.numeric(sub(",", ".", x, fixed = TRUE)))

quality_specifications <- function() {
  list(
    personnels_1d = list(
      key = c("annee_de_la_rentree_scolaire", "identifiant_de_l_etablissement"),
      time = "annee_de_la_rentree_scolaire", expected_time = c("2024", "2025"),
      geo = c(code_departement = "^([0-9]{2,3}|2A|2B)$", code_academie = "^[0-9]{2}$"),
      identifiers = c(identifiant_de_l_etablissement = "^[0-9A-Z]{8}$"),
      modalities = list(secteur = c("Public", "Privé sous contrat")),
      nonnegative = grep("^etp_", c("etp_d_enseignants_hommes_et_femmes", "etp_de_femmes_enseignantes",
        "etp_d_enseignants_de_moins_de_35_ans", "etp_d_enseignants_de_35_a_moins_de_50_ans",
        "etp_d_enseignants_de_50_ans_ou_plus"), value = TRUE),
      relations = list(
        c("etp_de_femmes_enseignantes", "<=", "etp_d_enseignants_hommes_et_femmes"),
        c("etp_d_enseignants_de_moins_de_35_ans", "+", "etp_d_enseignants_de_35_a_moins_de_50_ans",
          "+", "etp_d_enseignants_de_50_ans_ou_plus", "~=", "etp_d_enseignants_hommes_et_femmes"))
    ),
    personnels_2d = list(
      key = c("annee_de_la_rentree_scolaire", "identifiant_de_l_etablissement"),
      time = "annee_de_la_rentree_scolaire", expected_time = "2024",
      geo = c(code_departement = "^([0-9]{2,3}|2A|2B)$", code_academie = "^[0-9]{2}$"),
      identifiers = c(identifiant_de_l_etablissement = "^[0-9A-Z]{8}$"),
      modalities = list(secteur = c("Public", "Privé")),
      nonnegative = c("etp_total", "etp_de_personnels_de_vie_scolaire", "etp_enseignants_hommes_et_femmes",
                      "etp_de_femmes_enseignantes"),
      proportions = grep("^(proportion_|anciennete_)", c("proportion_femmes_enseignantes", "proportion_agreges",
        "proportion_certifies", "proportion_non_titulaires", "proportion_moins_de_35_ans",
        "proportion_35_50_ans", "proportion_plus_de_50_ans", "anciennete_moins_de_2_ans",
        "anciennete_2_a_5_ans", "anciennete_5_a_8_ans", "anciennete_8_ans"), value = TRUE),
      relations = list(c("etp_de_femmes_enseignantes", "<=", "etp_enseignants_hommes_et_femmes"))
    ),
    mouvement_1d = list(
      key = c("annee", "code_departement"), time = "annee", expected_time = "2019",
      geo = c(code_departement = "^([0-9]{2,3}|2A|2B)$"),
      modalities = list(),
      nonnegative = grep("^ratio_", c("ratio_demandes_entree_1er_voeu_demandes_sorties",
        "ratio_sorties_realisees_sorties_demandees_ens_tit_1d_public",
        "ratio_entrees_sorties_ens_tit_1d_public"), value = TRUE)
    ),
    annuaire_education = list(
      key = "identifiant_de_l_etablissement", time = NULL,
      geo = c(code_commune = "^[0-9AB]{5}$", code_departement = "^([0-9]{2,3}|0?2A|0?2B)$",
              code_academie = "^[0-9]{2}$"),
      identifiers = c(identifiant_de_l_etablissement = "^[0-9A-Z]{8}$"),
      modalities = list(statut_public_prive = c("Public", "Privé")), nonnegative = character()
    ),
    ips_ecoles = list(
      key = c("rentree_scolaire", "uai"), time = "rentree_scolaire",
      expected_time = c("2022-2023", "2023-2024", "2024-2025"),
      geo = c(code_insee_de_la_commune = "^[0-9AB]{5}$", code_du_departement = "^([0-9]{2,3}|0?2A|0?2B)$"),
      identifiers = c(uai = "^[0-9A-Z]{8}$"),
      modalities = list(secteur = c("public", "privé sous contrat")),
      nonnegative = grep("^ips", c("ips", "ips_national_prive", "ips_national_public", "ips_national",
        "ips_academique_prive", "ips_academique_public", "ips_academique",
        "ips_departemental_prive", "ips_departemental_public", "ips_departemental"), value = TRUE)
    ),
    ips_colleges = list(
      key = c("rentree_scolaire", "uai"), time = "rentree_scolaire", expected_time = "2022-2023",
      geo = c(code_insee_de_la_commune = "^[0-9AB]{5}$", code_du_departement = "^([0-9]{2,3}|0?2A|0?2B)$"),
      identifiers = c(uai = "^[0-9A-Z]{8}$"),
      modalities = list(secteur = c("public", "privé sous contrat")),
      nonnegative = c("effectifs", "ips", "ecart_type_de_l_ips")
    ),
    ips_lycees = list(
      key = c("rentree_scolaire", "uai"), time = "rentree_scolaire", expected_time = "2022-2023",
      geo = c(code_insee_de_la_commune = "^[0-9AB]{5}$", code_du_departement = "^([0-9]{2,3}|0?2A|0?2B)$"),
      identifiers = c(uai = "^[0-9A-Z]{8}$"),
      modalities = list(secteur = c("public", "privé sous contrat")),
      nonnegative = grep("^(effectifs|ips_|ecart_type)", c("effectifs_voie_gt", "effectifs_voie_pro",
        "effectifs_ensemble_gt_pro", "ips_voie_gt", "ips_voie_pro", "ips_ensemble_gt_pro",
        "ecart_type_de_l_ips_voie_gt", "ecart_type_de_l_ips_voie_pro"), value = TRUE),
      relations = list(c("effectifs_voie_gt", "+", "effectifs_voie_pro", "=", "effectifs_ensemble_gt_pro"))
    )
  )
}

audit_one_source <- function(data, source_id, spec) {
  out <- list(); add <- function(...) out[[length(out) + 1L]] <<- quality_result(source_id, ...)
  add("dimensions", "Table non vide", sprintf("%s lignes × %s colonnes", nrow(data), ncol(data)),
      "> 0 ligne et > 0 colonne", if (nrow(data) && ncol(data)) "OK" else "ERREUR", "Aucune" )

  key_missing <- sum(!stats::complete.cases(data[spec$key]))
  key_duplicates <- sum(duplicated(data[spec$key]))
  add("clés", "Valeurs manquantes dans la clé", paste(key_missing, "ligne(s)"), "0", if (key_missing == 0) "OK" else "ERREUR",
      if (key_missing) "Qualifier les lignes; ne pas imputer silencieusement" else "Aucune")
  add("doublons", "Unicité de la clé candidate", paste(key_duplicates, "doublon(s)"), "0", if (key_duplicates == 0) "OK" else "ALERTE",
      if (key_duplicates) "Expertiser la granularité avant dédoublonnage" else "Aucune")

  non_character <- names(data)[!vapply(data, is.character, logical(1))]
  add("types", "Import conservatoire en chaînes", if (length(non_character)) paste(non_character, collapse = ", ") else "Toutes les colonnes sont character",
      "100 % character au stade raw", if (!length(non_character)) "OK" else "ALERTE", "Typer seulement à l'étape de nettoyage")

  na_rate <- if (length(data)) sum(is.na(data)) / (nrow(data) * ncol(data)) else NA_real_
  add("NA", "Taux global de valeurs NA", sprintf("%.2f %%", 100 * na_rate), "Information; alerte si > 50 %",
      if (is.na(na_rate)) "ERREUR" else if (na_rate > .5) "ALERTE" else "OK",
      if (!is.na(na_rate) && na_rate > .5) "Documenter les variables structurellement absentes" else "Aucune")

  for (variable in names(spec$modalities)) {
    observed <- sort(unique(data[[variable]][!is.na(data[[variable]])]))
    unexpected <- setdiff(observed, spec$modalities[[variable]])
    add("modalités", paste("Nomenclature de", variable), paste(observed, collapse = " | "),
        paste(spec$modalities[[variable]], collapse = " | "), if (!length(unexpected)) "OK" else "ALERTE",
        if (length(unexpected)) paste("Documenter les modalités inattendues:", paste(unexpected, collapse = ", ")) else "Aucune")
  }

  numeric_columns <- intersect(unique(c(spec$nonnegative, spec$proportions)), names(data))
  missing_codes <- c("ss", "NS")
  invalid_numeric <- sum(vapply(data[numeric_columns], function(x) {
    value <- trimws(x); sum(!is.na(value) & nzchar(value) & !value %in% missing_codes & is.na(as_number_quality(value)))
  }, integer(1)))
  add("types", "Syntaxe numérique des mesures", paste(invalid_numeric, "valeur(s) non convertible(s)"), "0", if (invalid_numeric == 0) "OK" else "ALERTE",
      if (invalid_numeric) "Conserver et qualifier les chaînes concernées" else "Aucune")
  suppressed <- sum(vapply(data[numeric_columns], function(x) sum(trimws(x) %in% missing_codes, na.rm = TRUE), integer(1)))
  add("NA", "Marqueurs de secret ou non-significativité (ss/NS)", paste(suppressed, "valeur(s)"), "Information",
      if (suppressed) "INFORMATION" else "OK", if (suppressed) "Conserver comme indisponibilité documentée; ne pas imputer" else "Aucune")
  negative <- sum(vapply(data[intersect(spec$nonnegative, names(data))], function(x) sum(as_number_quality(x) < 0, na.rm = TRUE), integer(1)))
  add("valeurs aberrantes", "Mesures déclarées non négatives", paste(negative, "valeur(s) négative(s)"), "0", if (negative == 0) "OK" else "ALERTE",
      if (negative) "Expertiser; ne pas tronquer automatiquement" else "Aucune")
  if (length(spec$proportions)) {
    prop_bad <- sum(vapply(data[intersect(spec$proportions, names(data))], function(x) {z <- as_number_quality(x); sum(z < 0 | z > 100, na.rm = TRUE)}, integer(1)))
    add("valeurs aberrantes", "Pourcentages compris entre 0 et 100", paste(prop_bad, "valeur(s) hors plage"), "0", if (prop_bad == 0) "OK" else "ALERTE",
        if (prop_bad) "Vérifier l'unité publiée" else "Aucune")
  }

  if (!is.null(spec$time)) {
    periods <- sort(unique(data[[spec$time]][!is.na(data[[spec$time]])]))
    missing_periods <- setdiff(spec$expected_time, periods)
    add("cohérence temporelle", "Périodes attendues présentes", paste(periods, collapse = " | "), paste(spec$expected_time, collapse = " | "),
        if (!length(missing_periods)) "OK" else "ALERTE", if (length(missing_periods)) paste("Documenter les périodes absentes:", paste(missing_periods, collapse = ", ")) else "Aucune")
    counts <- table(data[[spec$time]])
    if (length(counts) >= 2) {
      change <- max(abs(diff(as.numeric(counts)) / head(as.numeric(counts), -1)), na.rm = TRUE)
      add("ruptures de série", "Variation des effectifs entre périodes consécutives", sprintf("Maximum %.2f %%", 100 * change), "Alerte si > 20 %",
          if (change > .2) "ALERTE" else "OK", if (change > .2) "Rechercher un changement de champ ou de collecte" else "Aucune")
    } else add("ruptures de série", "Variation entre périodes", "Une seule période disponible", "Au moins 2 périodes",
               "NON ÉVALUABLE", "Ne pas conclure à l'absence de rupture")
  }

  patterns <- c(spec$geo, spec$identifiers)
  for (variable in names(patterns)) if (variable %in% names(data)) {
    values <- trimws(data[[variable]]); invalid <- sum(!is.na(values) & nzchar(values) & !grepl(patterns[[variable]], values))
    category <- if (variable %in% names(spec$geo)) "cohérence géographique" else "nomenclatures"
    add(category, paste("Format de", variable), paste(invalid, "valeur(s) non conforme(s)"), patterns[[variable]],
        if (invalid == 0) "OK" else "ALERTE", if (invalid) "Comparer à la nomenclature officielle" else "Aucune")
  }

  for (relation in spec$relations) {
    if ("<=" %in% relation) {
      left <- as_number_quality(data[[relation[1]]]); right <- as_number_quality(data[[relation[3]]])
      violations <- sum(left > right, na.rm = TRUE); label <- paste(relation, collapse = " ")
    } else if ("~=" %in% relation) {
      total <- Reduce(`+`, lapply(relation[c(1, 3, 5)], function(v) as_number_quality(data[[v]])))
      reference <- as_number_quality(data[[relation[7]]]); violations <- sum(abs(total - reference) > .21, na.rm = TRUE); label <- paste(relation, collapse = " ")
    } else {
      total <- as_number_quality(data[[relation[1]]]) + as_number_quality(data[[relation[3]]])
      reference <- as_number_quality(data[[relation[5]]]); violations <- sum(abs(total - reference) > .01, na.rm = TRUE); label <- paste(relation, collapse = " ")
    }
    add("cohérence inter-variables", label, paste(violations, "violation(s)"), "0", if (violations == 0) "OK" else "ALERTE",
        if (violations) "Expertiser les écarts; aucune correction automatique" else "Aucune")
  }
  do.call(rbind, out)
}

run_quality_audit <- function(manifest = read_source_manifest(), import_results = NULL,
                              root = here::here()) {
  specs <- quality_specifications(); reports <- vector("list", length(manifest$sources))
  for (i in seq_along(manifest$sources)) {
    source <- manifest$sources[[i]]; data <- import_raw_csv(source_paths(source, root)$raw, source)
    reports[[i]] <- audit_one_source(data, source$id, specs[[source$id]])
  }
  report <- do.call(rbind, reports)
  write_utf8_csv(report, file.path(root, "outputs", "tables", "diagnostic_qualite.csv"))
  report
}
