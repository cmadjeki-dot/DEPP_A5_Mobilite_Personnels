# Construction de variables justifiees par les donnees publiques disponibles.

safe_ratio <- function(numerator, denominator) {
  result <- rep(NA_real_, length(numerator))
  valid <- !is.na(numerator) & !is.na(denominator) & denominator > 0
  result[valid] <- numerator[valid] / denominator[valid]
  result
}

education_priority_lookup <- function(annuaire) {
  if (!all(c("uai", "appartenance_education_prioritaire") %in% names(annuaire))) {
    stop("Variables d'éducation prioritaire absentes de l'annuaire.", call. = FALSE)
  }
  groups <- split(annuaire$appartenance_education_prioritaire, annuaire$uai)
  dispositif <- vapply(groups, function(value) {
    observed <- unique(value[!is.na(value)])
    if (length(observed) > 1L) return("CONFLIT")
    if (!length(observed)) return("HORS_EP")
    if (observed == "REP+") "REP_PLUS" else observed
  }, character(1))
  data.frame(uai = names(dispositif), dispositif_education_prioritaire = unname(dispositif),
             stringsAsFactors = FALSE)
}

append_education_priority <- function(data, lookup) {
  index <- match(data$uai, lookup$uai)
  matched <- !is.na(index)
  dispositif <- rep(NA_character_, nrow(data))
  dispositif[matched] <- lookup$dispositif_education_prioritaire[index[matched]]
  data$appariement_annuaire <- matched
  data$dispositif_education_prioritaire <- dispositif
  data$education_prioritaire <- ifelse(matched, dispositif %in% c("REP", "REP_PLUS"), NA)
  data
}

build_personnel_1d_features <- function(data, education_lookup) {
  total <- data$etp_d_enseignants_hommes_et_femmes
  result <- data.frame(
    annee = data$annee, uai = data$uai, secteur = data$secteur,
    code_departement = data$code_departement, code_academie = data$code_academie,
    code_region = data$code_region, etp_enseignants = total,
    part_femmes = safe_ratio(data$etp_de_femmes_enseignantes, total),
    part_age_moins_35 = safe_ratio(data$etp_d_enseignants_de_moins_de_35_ans, total),
    part_age_35_49 = safe_ratio(data$etp_d_enseignants_de_35_a_moins_de_50_ans, total),
    part_age_50_plus = safe_ratio(data$etp_d_enseignants_de_50_ans_ou_plus, total),
    part_anciennete_etab_moins_2 = safe_ratio(data$etp_d_enseignants_ayant_une_anciennete_dans_l_etablissement_de_moins_de_2_ans, total),
    part_anciennete_etab_2_4 = safe_ratio(data$etp_d_enseignants_ayant_une_anciennete_dans_l_etablissement_de_2_ans_a_moins_de_5_ans, total),
    part_anciennete_etab_5_7 = safe_ratio(data$etp_d_enseignants_ayant_une_anciennete_dans_l_etablissement_de_5_ans_a_moins_de_8_ans, total),
    part_anciennete_etab_8_plus = safe_ratio(data$etp_d_enseignants_ayant_une_anciennete_dans_l_etablissement_de_8_ans_ou_plus, total),
    stringsAsFactors = FALSE
  )
  result$proxy_stabilite_etablissement <- result$part_anciennete_etab_8_plus
  result$proxy_renouvellement_etablissement <- result$part_anciennete_etab_moins_2
  append_education_priority(result, education_lookup)
}

build_personnel_2d_features <- function(data, education_lookup) {
  percent <- function(variable) data[[variable]] / 100
  result <- data.frame(
    annee = data$annee, uai = data$uai, secteur = data$secteur,
    code_departement = data$code_departement, code_academie = data$code_academie,
    etp_personnels = data$etp_total, etp_enseignants = data$etp_enseignants_hommes_et_femmes,
    part_femmes = percent("proportion_femmes_enseignantes"),
    part_age_moins_35 = percent("proportion_moins_de_35_ans"),
    part_age_35_49 = percent("proportion_35_50_ans"),
    part_age_50_plus = percent("proportion_plus_de_50_ans"),
    part_anciennete_etab_moins_2 = percent("anciennete_moins_de_2_ans"),
    part_anciennete_etab_2_4 = percent("anciennete_2_a_5_ans"),
    part_anciennete_etab_5_7 = percent("anciennete_5_a_8_ans"),
    part_anciennete_etab_8_plus = percent("anciennete_8_ans"),
    part_agreges = percent("proportion_agreges"),
    part_certifies_peps = percent("proportion_certifies"),
    part_non_titulaires = percent("proportion_non_titulaires"),
    stringsAsFactors = FALSE
  )
  result$proxy_stabilite_etablissement <- result$part_anciennete_etab_8_plus
  result$proxy_renouvellement_etablissement <- result$part_anciennete_etab_moins_2
  append_education_priority(result, education_lookup)
}

build_mobility_features <- function(data) {
  data.frame(
    annee = data$annee,
    code_departement = data$code_departement,
    nom_departement = data$nom_departement,
    pression_demandes_entree_sortie = data$ratio_demandes_entree_1er_voeu_demandes_sorties,
    ratio_sorties_realisees_demandees = data$ratio_sorties_realisees_sorties_demandees_ens_tit_1d_public,
    ratio_entrees_sorties_realisees = data$ratio_entrees_sorties_ens_tit_1d_public,
    stringsAsFactors = FALSE
  )
}

feature_dictionary <- function() {
  data.frame(
    NOM = c(
      "age_individuel", "classe_age_individuelle", "part_age_moins_35", "part_age_35_49", "part_age_50_plus",
      "anciennete_individuelle", "classe_anciennete_individuelle", "part_anciennete_etab_moins_2",
      "part_anciennete_etab_2_4", "part_anciennete_etab_5_7", "part_anciennete_etab_8_plus",
      "proxy_stabilite_etablissement", "proxy_renouvellement_etablissement", "code_departement",
      "code_academie", "code_region", "dispositif_education_prioritaire", "education_prioritaire",
      "pression_demandes_entree_sortie", "ratio_sorties_realisees_demandees",
      "ratio_entrees_sorties_realisees", "changement_etablissement", "changement_departement",
      "changement_academie", "changement_fonction", "mobilite_individuelle", "part_agreges",
      "part_certifies_peps", "part_non_titulaires"
    ),
    "DÉFINITION" = c(
      "Âge d'un enseignant", "Classe d'âge d'un enseignant", "Part des ETP enseignants de moins de 35 ans",
      "Part des ETP de 35 à 49 ans", "Part des ETP de 50 ans ou plus", "Ancienneté professionnelle individuelle",
      "Classe d'ancienneté individuelle", "Part des ETP présents dans l'établissement depuis moins de 2 ans",
      "Part depuis 2 à 4 ans", "Part depuis 5 à 7 ans", "Part depuis 8 ans ou plus",
      "Proxy agrégé de stabilité dans l'établissement", "Proxy agrégé de renouvellement récent",
      "Territoire départemental de l'établissement", "Territoire académique de l'établissement",
      "Région de l'établissement lorsqu'elle est publiée", "Dispositif REP, REP+ ou hors EP au snapshot de l'annuaire",
      "Indicateur d'appartenance à l'éducation prioritaire", "Rapport demandes d'entrée en premier vœu / demandes de sortie",
      "Rapport sorties réalisées / sorties demandées", "Rapport entrées réalisées / sorties réalisées",
      "Changement d'UAI entre t et t+1", "Changement de département entre t et t+1",
      "Changement d'académie entre t et t+1", "Changement de fonction entre t et t+1",
      "Mobilité d'un enseignant entre t et t+1", "Part agrégée d'enseignants agrégés",
      "Part agrégée de certifiés et PEPS", "Part agrégée d'enseignants non titulaires"
    ),
    FORMULE = c(
      rep("Non construite", 2),
      "ETP moins de 35 ans / ETP enseignants", "ETP 35-49 ans / ETP enseignants", "ETP 50 ans ou plus / ETP enseignants",
      "Non construite", "Non construite",
      "ETP ancienneté <2 / ETP enseignants", "ETP ancienneté 2-4 / ETP enseignants",
      "ETP ancienneté 5-7 / ETP enseignants", "ETP ancienneté 8+ / ETP enseignants",
      "part_anciennete_etab_8_plus", "part_anciennete_etab_moins_2", "Code harmonisé publié",
      "Code harmonisé publié", "Code harmonisé publié", "Modalité annuaire harmonisée; blanc interprété HORS_EP",
      "dispositif dans {REP, REP_PLUS}", "Ratio officiel publié", "Ratio officiel publié", "Ratio officiel publié",
      rep("Non construite", 5), "proportion_agreges / 100", "proportion_certifies / 100", "proportion_non_titulaires / 100"
    ),
    SOURCE = c(
      rep("Non disponible dans l'open data", 2), rep("Personnels 1D/2D", 3),
      rep("Non disponible dans l'open data", 2), rep("Personnels 1D/2D", 6),
      "Personnels 1D/2D", "Personnels 1D/2D", "Personnels 1D", "Annuaire de l'éducation",
      "Annuaire de l'éducation", rep("Mouvement interdépartemental 1D public 2019", 3),
      rep("Panel individuel BSA requis", 5), rep("Personnels 2D", 3)
    ),
    JUSTIFICATION = c(
      "Aucune date de naissance publique", "Aucune observation individuelle", rep("Composition par âge publiée", 3),
      "Aucune date d'entrée ou ancienneté de carrière", "Aucune observation individuelle", rep("Ancienneté établissement publiée par classes", 4),
      "Approche prudente de la stabilité", "Approche prudente du renouvellement", rep("Nomenclature territoriale disponible", 3),
      "Contexte organisationnel appariable par UAI", "Variable contextuelle appariable par UAI",
      "Mesure publiée de pression relative", "Mesure publiée de satisfaction des sorties", "Mesure publiée d'équilibre relatif des flux",
      rep("Nécessite deux positions individuelles successives", 4), "Nécessite une cible individuelle longitudinale",
      rep("Composition professionnelle publiée", 3)
    ),
    "NA" = c(
      rep("100 % : variable absente", 2), rep("NA si ETP masqué/manquant ou dénominateur nul", 3),
      rep("100 % : variable absente", 2), rep("NA si ETP masqué/manquant ou dénominateur nul", 6),
      rep("NA si code source manquant", 3), "NA si UAI non apparié", "NA si UAI non apparié",
      rep("NA source conservé", 3), rep("100 % : non constructible", 5), rep("NA source conservé", 3)
    ),
    LIMITE = c(
      rep("Microdonnée individuelle interne nécessaire", 2), rep("Indicateur agrégé; aucune inférence individuelle", 3),
      rep("Microdonnée individuelle interne nécessaire", 2), rep("Classes agrégées, pas ancienneté exacte", 4),
      "Ancienneté élevée ne prouve pas l'absence de mobilité", "Ancienneté faible peut inclure créations ou recompositions",
      rep("Nomenclature, pas caractéristique individuelle", 3),
      "Snapshot 2026; le blanc est interprété hors EP", "Snapshot 2026, non historique",
      "Ratio sans effectifs bruts ni taux individuel", "Ne mesure pas toutes les sorties", "Ne mesure pas un taux individuel de mobilité",
      rep("Impossible avec les données publiques acquises", 5),
      rep("Composition d'établissement, pas corps individuel", 3)
    ), stringsAsFactors = FALSE, check.names = FALSE
  )
}

validate_feature_table <- function(data, key, source_id) {
  if (anyDuplicated(data[key])) stop("Clé non unique dans les indicateurs: ", source_id, call. = FALSE)
  share_columns <- grep("^(part_|proxy_)", names(data), value = TRUE)
  invalid <- sum(vapply(data[share_columns], function(x) sum(x < 0 | x > 1, na.rm = TRUE), integer(1)))
  if (invalid) stop("Parts hors [0,1] dans ", source_id, ": ", invalid, call. = FALSE)
  if ("dispositif_education_prioritaire" %in% names(data)) {
    allowed <- c("REP", "REP_PLUS", "HORS_EP", NA_character_)
    if (length(setdiff(unique(data$dispositif_education_prioritaire), allowed))) stop("Modalité EP inattendue.", call. = FALSE)
  }
  invisible(TRUE)
}

feature_validation_report <- function(features_1d, features_2d, mobility, lookup) {
  row <- function(indicator, rule, result, threshold, status, action) {
    data.frame(INDICATEUR = indicator, REGLE = rule, RESULTAT = result, SEUIL = threshold,
               STATUT = status, ACTION = action, stringsAsFactors = FALSE)
  }
  range_violations <- function(data) {
    columns <- grep("^(part_|proxy_)", names(data), value = TRUE)
    sum(vapply(data[columns], function(x) sum(x < 0 | x > 1, na.rm = TRUE), integer(1)))
  }
  composition_violations <- function(data, columns, tolerance = .07) {
    total <- Reduce(`+`, data[columns])
    sum(abs(total - 1) > tolerance, na.rm = TRUE)
  }
  unavailable <- c("age_individuel", "anciennete_individuelle", "changement_etablissement",
                   "changement_departement", "changement_academie", "changement_fonction", "mobilite_individuelle")
  present_unavailable <- intersect(unavailable, union(names(features_1d), names(features_2d)))
  reports <- list(
    row("Clé personnels 1D", "Unicité annee × uai", sum(duplicated(features_1d[c("annee", "uai")])), "0", "OK", "Aucune"),
    row("Clé personnels 2D", "Unicité annee × uai", sum(duplicated(features_2d[c("annee", "uai")])), "0", "OK", "Aucune"),
    row("Clé mobilité", "Unicité annee × code_departement", sum(duplicated(mobility[c("annee", "code_departement")])), "0", "OK", "Aucune"),
    row("Bornes des parts 1D", "Toutes les parts dans [0,1]", range_violations(features_1d), "0", "OK", "Aucune"),
    row("Bornes des parts 2D", "Toutes les parts dans [0,1]", range_violations(features_2d), "0", "OK", "Aucune"),
    row("Classes d'âge 1D", "Somme des trois parts = 1 ± 0,07 (arrondis publiés)", composition_violations(features_1d,
      c("part_age_moins_35", "part_age_35_49", "part_age_50_plus")), "0", "OK", "Aucune"),
    row("Classes d'âge 2D", "Somme des trois parts = 1 ± 0,07 (arrondis publiés)", composition_violations(features_2d,
      c("part_age_moins_35", "part_age_35_49", "part_age_50_plus")), "0", "OK", "Aucune"),
    row("Ancienneté établissement 1D", "Somme des quatre parts = 1 ± 0,07 (arrondis publiés)", composition_violations(features_1d,
      c("part_anciennete_etab_moins_2", "part_anciennete_etab_2_4", "part_anciennete_etab_5_7", "part_anciennete_etab_8_plus")), "0", "OK", "Aucune"),
    row("Ancienneté établissement 2D", "Somme des quatre parts = 1 ± 0,07 (arrondis publiés)", composition_violations(features_2d,
      c("part_anciennete_etab_moins_2", "part_anciennete_etab_2_4", "part_anciennete_etab_5_7", "part_anciennete_etab_8_plus")), "0", "OK", "Aucune"),
    row("Nomenclature éducation prioritaire", "Aucun conflit REP/REP+ par UAI", sum(lookup$dispositif_education_prioritaire == "CONFLIT"), "0", "OK", "Aucune"),
    row("Appariement annuaire 1D", "Part des lignes avec UAI retrouvé", sprintf("%.2f %%", 100 * mean(features_1d$appariement_annuaire)), "Information", "INFORMATION", "Conserver NA pour les UAI non appariés"),
    row("Appariement annuaire 2D", "Part des lignes avec UAI retrouvé", sprintf("%.2f %%", 100 * mean(features_2d$appariement_annuaire)), "Information", "INFORMATION", "Conserver NA pour les UAI non appariés"),
    row("Variables longitudinales individuelles", "Aucune variable non observable n'est fabriquée",
        if (length(present_unavailable)) paste(present_unavailable, collapse = ", ") else "Aucune variable créée",
        "0 variable", if (length(present_unavailable)) "ERREUR" else "OK",
        "Panel individuel interne requis pour les construire"),
    row("Temporalité éducation prioritaire", "Millésime compatible avec l'année des personnels",
        "Annuaire snapshot 2026 apparié aux personnels 2024-2025", "Même millésime souhaitable", "LIMITE",
        "Interpréter comme contexte récent, pas comme statut historique certain")
  )
  report <- do.call(rbind, reports)
  numeric_result <- suppressWarnings(as.numeric(report$RESULTAT[report$SEUIL == "0"]))
  if (any(numeric_result != 0, na.rm = TRUE)) report$STATUT[report$SEUIL == "0" & suppressWarnings(as.numeric(report$RESULTAT)) != 0] <- "ERREUR"
  report
}

run_feature_engineering <- function(cleaned_data = NULL, root = here::here()) {
  clean_directory <- file.path(root, "data", "interim", "clean")
  output_directory <- file.path(root, "data", "processed", "features")
  fs::dir_create(output_directory)
  annuaire <- readRDS(file.path(clean_directory, "annuaire_education_clean.rds"))
  personnel_1d <- readRDS(file.path(clean_directory, "personnels_1d_clean.rds"))
  personnel_2d <- readRDS(file.path(clean_directory, "personnels_2d_clean.rds"))
  mouvement <- readRDS(file.path(clean_directory, "mouvement_1d_clean.rds"))
  lookup <- education_priority_lookup(annuaire)
  features_1d <- build_personnel_1d_features(personnel_1d, lookup)
  features_2d <- build_personnel_2d_features(personnel_2d, lookup)
  mobility <- build_mobility_features(mouvement)
  validate_feature_table(features_1d, c("annee", "uai"), "personnels_1d")
  validate_feature_table(features_2d, c("annee", "uai"), "personnels_2d")
  validate_feature_table(mobility, c("annee", "code_departement"), "mouvement_1d")
  validation <- feature_validation_report(features_1d, features_2d, mobility, lookup)
  if (any(validation$STATUT == "ERREUR")) stop("La validation des indicateurs a échoué.", call. = FALSE)
  paths <- c(personnels_1d = file.path(output_directory, "personnels_1d_features.rds"),
             personnels_2d = file.path(output_directory, "personnels_2d_features.rds"),
             mobilite_1d = file.path(output_directory, "mobilite_1d_features.rds"))
  saveRDS(features_1d, paths[["personnels_1d"]], version = 3)
  saveRDS(features_2d, paths[["personnels_2d"]], version = 3)
  saveRDS(mobility, paths[["mobilite_1d"]], version = 3)
  dictionary <- feature_dictionary()
  write_utf8_csv(dictionary, file.path(output_directory, "dictionnaire_variables.csv"))
  write_utf8_csv(validation, file.path(output_directory, "validation_indicateurs.csv"))
  summary <- data.frame(table = names(paths), rows = c(nrow(features_1d), nrow(features_2d), nrow(mobility)),
                        columns = c(ncol(features_1d), ncol(features_2d), ncol(mobility)), path = unname(paths))
  write_utf8_csv(summary, file.path(output_directory, "features_summary.csv"))
  list(files = paths, summary = summary, dictionary = dictionary, validation = validation)
}
