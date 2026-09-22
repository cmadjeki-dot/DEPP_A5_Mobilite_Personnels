# Construction longitudinale sous contrainte de granularite ouverte.

assess_person_year_feasibility <- function(data) {
  required <- c("identifiant_personnel", "annee", "uai", "code_departement", "code_academie", "fonction")
  available <- required %in% names(data)
  data.frame(
    CRITERE = c("Identifiant individuel stable", "Année", "Établissement", "Département", "Académie", "Fonction",
                "Unité personnel × année"),
    VARIABLE = c(required, "ensemble des critères"),
    DISPONIBLE = c(available, all(available)),
    CONCLUSION = c(ifelse(available, "Disponible", "Absent"),
                   if (all(available)) "Panel individuel réalisable" else "Panel individuel non réalisable"),
    stringsAsFactors = FALSE
  )
}

build_establishment_panel <- function(data) {
  required <- c("annee", "uai", "code_departement", "code_academie", "etp_enseignants")
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("Variables absentes pour le panel établissement: ", paste(missing, collapse = ", "), call. = FALSE)
  if (anyDuplicated(data[c("annee", "uai")])) stop("Doublons sur la clé uai × annee.", call. = FALSE)

  minimum_year <- min(data$annee, na.rm = TRUE)
  maximum_year <- max(data$annee, na.rm = TRUE)
  panel <- data |>
    dplyr::arrange(uai, annee) |>
    dplyr::group_by(uai) |>
    dplyr::mutate(
      annee_precedente = dplyr::lag(annee),
      annee_suivante = dplyr::lead(annee),
      departement_precedent = dplyr::lag(code_departement),
      academie_precedente = dplyr::lag(code_academie),
      etp_enseignants_precedent = dplyr::lag(etp_enseignants),
      observation_precedente = !is.na(annee_precedente),
      observation_suivante = !is.na(annee_suivante),
      annees_manquantes_avant = ifelse(!is.na(annee_precedente), pmax(annee - annee_precedente - 1L, 0L), NA_integer_),
      sequence_consecutive = !is.na(annee_precedente) & annee == annee_precedente + 1L,
      censure_gauche = annee == minimum_year & is.na(annee_precedente),
      censure_droite = annee == maximum_year & is.na(annee_suivante),
      apparition_dans_fichier = annee > minimum_year & is.na(annee_precedente),
      disparition_apres_observation = annee < maximum_year & is.na(annee_suivante),
      changement_code_departement_etablissement = sequence_consecutive & !is.na(code_departement) &
        !is.na(departement_precedent) & code_departement != departement_precedent,
      changement_code_academie_etablissement = sequence_consecutive & !is.na(code_academie) &
        !is.na(academie_precedente) & code_academie != academie_precedente,
      variation_etp_enseignants = ifelse(sequence_consecutive, etp_enseignants - etp_enseignants_precedent, NA_real_),
      taux_variation_etp_enseignants = ifelse(sequence_consecutive & etp_enseignants_precedent > 0,
        (etp_enseignants - etp_enseignants_precedent) / etp_enseignants_precedent, NA_real_)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(statut_observation = dplyr::case_when(
      apparition_dans_fichier ~ "APPARITION_DANS_FICHIER",
      disparition_apres_observation ~ "DISPARITION_APRES_OBSERVATION",
      sequence_consecutive ~ "OBSERVATION_CONTINUE",
      TRUE ~ "BORNE_DE_FENETRE"
    ))
  as.data.frame(panel)
}

panel_transition_dictionary <- function() {
  data.frame(
    TRANSITION = c("stable", "mobilité établissement", "mobilité département", "mobilité académie",
                   "changement fonction", "entrée", "sortie", "apparition dans le fichier",
                   "disparition après observation", "variation des ETP"),
    STATUT = c(rep("NON CONSTRUCTIBLE", 7), rep("ALTERNATIVE ÉTABLISSEMENT", 3)),
    DEFINITION_OPERATIONNELLE = c(
      "Nécessite la même personne observée sans changement entre t et t+1",
      "Nécessite l'UAI de chaque personne en t et t+1", "Nécessite le département de chaque personne en t et t+1",
      "Nécessite l'académie de chaque personne en t et t+1", "Nécessite la fonction individuelle en t et t+1",
      "Nécessite une date ou un état d'entrée individuel", "Nécessite un motif de sortie individuel",
      "UAI absente avant sa première année observée dans la fenêtre", "UAI absente après sa dernière année observée dans la fenêtre",
      "Différence d'ETP enseignants d'un même établissement entre années consécutives"),
    INTERPRETATION_AUTORISEE = c(
      rep("Aucune estimation avec l'open data acquis", 7),
      "Présence nouvelle dans le fichier; ce n'est pas une entrée de personnel",
      "Perte de présence dans le fichier; ce n'est pas une sortie de personnel",
      "Évolution d'un agrégat; aucune trajectoire individuelle"),
    LIMITE = c(
      rep("Panel individuel BSA sécurisé requis", 7),
      "Création, réouverture, changement de champ ou défaut d'appariement possibles",
      "Fermeture, changement de champ ou défaut d'appariement possibles",
      "Solde agrégé incompatible avec l'identification des flux individuels"),
    stringsAsFactors = FALSE
  )
}

validate_establishment_panel <- function(panel, source_rows) {
  result <- list(); add <- function(indicator, rule, value, threshold, status, action) {
    result[[length(result) + 1L]] <<- data.frame(INDICATEUR = indicator, REGLE = rule, RESULTAT = as.character(value),
      SEUIL = threshold, STATUT = status, ACTION = action, stringsAsFactors = FALSE)
  }
  duplicates <- sum(duplicated(panel[c("annee", "uai")]))
  order_errors <- sum(vapply(split(panel$annee, panel$uai), function(x) is.unsorted(x), logical(1)))
  gaps <- sum(panel$annees_manquantes_avant > 0, na.rm = TRUE)
  forbidden <- intersect(c("stable", "mobilite_etablissement", "mobilite_departement", "mobilite_academie",
                           "changement_fonction", "entree_personnel", "sortie_personnel", "mobilite"), names(panel))
  add("Dimensions", "Même nombre de lignes que la source", nrow(panel), source_rows,
      if (nrow(panel) == source_rows) "OK" else "ERREUR", "Aucune")
  add("Identifiants", "Clé uai × annee unique", duplicates, "0", if (duplicates == 0) "OK" else "ERREUR", "Corriger avant analyse")
  add("Ordre temporel", "Années triées dans chaque UAI", order_errors, "0", if (order_errors == 0) "OK" else "ERREUR", "Trier avant décalage")
  add("Années manquantes", "Nombre de ruptures internes", gaps, "Information", "INFORMATION", "Ne pas créer de transition à travers un trou")
  add("Censure gauche", "Premières observations à la borne basse", sum(panel$censure_gauche), "Information", "INFORMATION", "Historique antérieur inconnu")
  add("Censure droite", "Dernières observations à la borne haute", sum(panel$censure_droite), "Information", "INFORMATION", "Devenir ultérieur inconnu")
  add("Apparitions", "UAI apparaissant après le début de fenêtre", sum(panel$apparition_dans_fichier), "Information", "INFORMATION", "Ne pas appeler entrée de personnel")
  add("Disparitions apparentes", "UAI absente avant la fin de fenêtre", sum(panel$disparition_apres_observation), "Information", "INFORMATION", "Ne pas appeler sortie de personnel")
  add("Variables interdites", "Aucune transition individuelle fabriquée", length(forbidden), "0",
      if (!length(forbidden)) "OK" else "ERREUR", "Retirer toute variable individuelle non identifiable")
  do.call(rbind, result)
}

run_panel_strategy <- function(feature_data = NULL, root = here::here()) {
  input <- file.path(root, "data", "processed", "features", "personnels_1d_features.rds")
  data <- readRDS(input)
  feasibility <- assess_person_year_feasibility(data)
  if (any(feasibility$VARIABLE == "identifiant_personnel" & feasibility$DISPONIBLE)) {
    stop("Le diagnostic de granularité doit être revu avant de construire un panel individuel.", call. = FALSE)
  }
  panel <- build_establishment_panel(data)
  validation <- validate_establishment_panel(panel, nrow(data))
  if (any(validation$STATUT == "ERREUR")) stop("Validation du panel alternatif échouée.", call. = FALSE)
  transitions <- panel_transition_dictionary()
  output_directory <- file.path(root, "data", "processed", "panel")
  fs::dir_create(output_directory)
  panel_path <- file.path(output_directory, "etablissement_annee_panel.rds")
  saveRDS(panel, panel_path, version = 3)
  write_utf8_csv(feasibility, file.path(output_directory, "faisabilite_panel_personnel.csv"))
  write_utf8_csv(transitions, file.path(output_directory, "dictionnaire_transitions.csv"))
  write_utf8_csv(validation, file.path(output_directory, "validation_panel.csv"))
  list(panel_file = panel_path, feasibility = feasibility, transitions = transitions, validation = validation,
       summary = data.frame(unit = "etablissement × annee", rows = nrow(panel), uai = length(unique(panel$uai)),
                            years = paste(sort(unique(panel$annee)), collapse = "-")))
}
