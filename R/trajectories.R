# Analyse de trajectoires sous contrainte de granularite ouverte.

professional_trajectory_feasibility <- function() {
  data.frame(
    OBJET = c("Séquences professionnelles", "Durée dans un état professionnel", "Transitions professionnelles",
              "Matrice de transition professionnelle", "Trajectoires professionnelles fréquentes", "Profils de parcours individuels"),
    DONNEES_REQUISES = c(
      "Identifiant personnel et état répété sur au moins trois dates",
      "Dates d'entrée et de sortie de chaque état individuel",
      "État individuel comparable en t et t+1", "Transitions individuelles observées",
      "Séquences individuelles complètes", "Covariables et séquences individuelles"),
    DISPONIBILITE = "NON DISPONIBLE",
    DECISION = "NON CONSTRUIT",
    JUSTIFICATION = c(
      "Aucun identifiant personnel public; deux années seulement au niveau UAI",
      "Aucune date d'état individuel", "Les variations d'ETP ne révèlent pas les mouvements individuels",
      "Une matrice agrégée ne serait pas une matrice de carrières",
      "Stable → Mobilité → Stable exige trois dates individuelles",
      "Le biais écologique interdit de transformer un profil d'établissement en profil de personne"),
    stringsAsFactors = FALSE
  )
}

build_establishment_presence_sequences <- function(panel) {
  years <- sort(unique(panel$annee))
  if (length(years) != 2L) stop("L'alternative actuelle exige exactement deux années observées.", call. = FALSE)
  if (anyDuplicated(panel[c("annee", "uai")])) stop("Clé uai × annee non unique.", call. = FALSE)
  minimum_year <- years[[1]]; maximum_year <- years[[2]]
  observed <- data.table::as.data.table(panel)[, .(uai, annee, etp_enseignants,
    education_prioritaire, dispositif_education_prioritaire)]
  grid <- data.table::CJ(uai = sort(unique(observed$uai)), annee = years)
  expanded <- merge(grid, observed, by = c("uai", "annee"), all.x = TRUE, sort = TRUE)
  expanded[, etat_presence := ifelse(is.na(etp_enseignants), "ABSENT_DU_FICHIER", "OBSERVE")]
  presence <- data.table::dcast(expanded, uai ~ annee, value.var = "etat_presence")
  initial_name <- as.character(minimum_year); final_name <- as.character(maximum_year)
  presence[, sequence_presence := paste(get(initial_name), get(final_name), sep = " → ")]
  presence[, duree_observee_annees := (get(initial_name) == "OBSERVE") + (get(final_name) == "OBSERVE")]
  presence[, `:=`(
    censure_gauche = get(initial_name) == "OBSERVE",
    censure_droite = get(final_name) == "OBSERVE"
  )]
  initial <- observed[annee == minimum_year, .(uai, etp_initial = etp_enseignants,
    ep_initial = education_prioritaire, dispositif_ep_initial = dispositif_education_prioritaire)]
  final <- observed[annee == maximum_year, .(uai, etp_final = etp_enseignants,
    ep_final = education_prioritaire, dispositif_ep_final = dispositif_education_prioritaire)]
  presence <- merge(presence, initial, by = "uai", all.x = TRUE, sort = FALSE)
  presence <- merge(presence, final, by = "uai", all.x = TRUE, sort = FALSE)
  presence[, variation_etp := etp_final - etp_initial]
  presence[, education_prioritaire_reference := data.table::fcoalesce(ep_final, ep_initial)]
  data.table::setorder(presence, uai)
  as.data.frame(presence)
}

build_presence_transition_matrix <- function(sequences) {
  year_columns <- grep("^[0-9]{4}$", names(sequences), value = TRUE)
  if (length(year_columns) != 2L) stop("Deux colonnes annuelles sont requises.", call. = FALSE)
  result <- data.table::as.data.table(sequences)[, .N,
    by = .(etat_origine = get(year_columns[[1]]), etat_destination = get(year_columns[[2]]))]
  result[, proportion_conditionnelle := N / sum(N), by = etat_origine]
  data.table::setorder(result, etat_origine, etat_destination)
  as.data.frame(result)
}

build_trajectory_profiles <- function(sequences) {
  result <- data.table::as.data.table(sequences)[, .(
    etablissements = .N,
    part_des_uai = .N / nrow(sequences),
    duree_observee_mediane = as.numeric(stats::median(duree_observee_annees)),
    etp_initial_median = as.numeric(stats::median(etp_initial, na.rm = TRUE)),
    etp_final_median = as.numeric(stats::median(etp_final, na.rm = TRUE)),
    variation_etp_mediane = as.numeric(stats::median(variation_etp, na.rm = TRUE)),
    part_education_prioritaire = mean(education_prioritaire_reference, na.rm = TRUE)
  ), by = sequence_presence]
  numeric_columns <- names(result)[vapply(result, is.numeric, logical(1))]
  for (variable in numeric_columns) result[is.nan(get(variable)), (variable) := NA_real_]
  data.table::setorder(result, -etablissements)
  as.data.frame(result)
}

trajectory_analysis_registers <- function() {
  data.frame(
    REGISTRE = c("Description", "Association", "Causalité"),
    QUESTION_AUTORISEE = c(
      "Combien d'UAI sont observées aux deux dates, apparaissent ou disparaissent du fichier ?",
      "Aucune association de carrière estimée à cette étape; une association agrégée resterait écologique",
      "Aucune question causale n'est identifiable avec ces données"),
    INTERPRETATION = c(
      "Présence dans le fichier et évolution d'ETP au niveau établissement",
      "Une corrélation entre caractéristiques d'établissement ne s'applique pas aux personnes",
      "Ni effet du territoire, ni effet de l'éducation prioritaire, ni cause de mobilité"),
    STATUT = c("RÉALISÉ", "NON RÉALISÉ", "NON IDENTIFIABLE"), stringsAsFactors = FALSE
  )
}

validate_trajectory_outputs <- function(sequences, matrix, source_panel) {
  allowed <- c("OBSERVE → OBSERVE", "OBSERVE → ABSENT_DU_FICHIER", "ABSENT_DU_FICHIER → OBSERVE")
  checks <- data.frame(
    INDICATEUR = c("Une ligne par UAI", "Séquences autorisées", "Total matrice",
                   "Profondeur temporelle", "Aucune mobilité individuelle fabriquée"),
    RESULTAT = c(nrow(sequences) - length(unique(source_panel$uai)),
      length(setdiff(unique(sequences$sequence_presence), allowed)), sum(matrix$N) - nrow(sequences),
      max(table(source_panel$uai)),
      length(intersect(c("mobilite", "stable", "sortie_personnel", "changement_fonction"), names(sequences)))),
    SEUIL = c("0", "0", "0", "Au moins 3 pour les exemples conceptuels", "0"),
    STATUT = c("OK", "OK", "OK", "LIMITE", "OK"),
    ACTION = c("Aucune", "Aucune", "Aucune", "Ne pas construire de séquence professionnelle à trois états", "Aucune"),
    stringsAsFactors = FALSE
  )
  zero_checks <- c(1, 2, 3, 5)
  checks$STATUT[zero_checks] <- ifelse(checks$RESULTAT[zero_checks] == 0, "OK", "ERREUR")
  checks
}

build_trajectory_plots <- function(profiles, matrix, root = here::here()) {
  output <- file.path(root, "outputs", "figures", "trajectories")
  fs::dir_create(output)
  sequence_plot <- ggplot2::ggplot(profiles,
    ggplot2::aes(stats::reorder(sequence_presence, etablissements), etablissements)) +
    ggplot2::geom_col(fill = "#35608D") + ggplot2::coord_flip() +
    descriptive_plot_labels("Séquences de présence des établissements", "UAI observées au moins une fois",
      "2024–2025", "Nombre d'établissements", "DEPP, indicateurs de personnels du premier degré",
      "Une apparition ou disparition du fichier n'est pas une entrée ou sortie de personnel.") +
    ggplot2::labs(x = NULL, y = "Établissements") + descriptive_theme()
  matrix_plot <- ggplot2::ggplot(matrix,
    ggplot2::aes(etat_origine, etat_destination, fill = proportion_conditionnelle)) +
    ggplot2::geom_tile(color = "white") + ggplot2::geom_text(ggplot2::aes(label = N), color = "black") +
    descriptive_plot_labels("Matrice descriptive de présence dans le fichier", "Union des UAI observées en 2024 ou 2025",
      "2024 → 2025", "Effectif et proportion conditionnelle d'UAI", "DEPP, indicateurs de personnels du premier degré",
      "La population est conditionnée à une présence au moins une fois; la case absent→absent n'est pas observable.") +
    ggplot2::labs(x = "État en 2024", y = "État en 2025", fill = "Proportion") + descriptive_theme()
  plots <- list(sequences = sequence_plot, matrice_presence = matrix_plot)
  vapply(names(plots), function(name) {
    path <- file.path(output, paste0(name, ".png"))
    ggplot2::ggsave(path, plots[[name]], width = 9, height = 5.5, dpi = 160, bg = "white")
    path
  }, character(1))
}

run_trajectory_analysis <- function(panel_data = NULL, root = here::here()) {
  panel <- readRDS(file.path(root, "data", "processed", "panel", "etablissement_annee_panel.rds"))
  feasibility <- professional_trajectory_feasibility()
  sequences <- build_establishment_presence_sequences(panel)
  matrix <- build_presence_transition_matrix(sequences)
  profiles <- build_trajectory_profiles(sequences)
  registers <- trajectory_analysis_registers()
  validation <- validate_trajectory_outputs(sequences, matrix, panel)
  if (any(validation$STATUT == "ERREUR")) stop("Validation des trajectoires alternatives échouée.", call. = FALSE)
  figures <- build_trajectory_plots(profiles, matrix, root)
  output <- file.path(root, "outputs", "tables", "trajectories")
  fs::dir_create(output)
  write_utf8_csv(feasibility, file.path(output, "faisabilite_trajectoires_professionnelles.csv"))
  write_utf8_csv(sequences, file.path(output, "sequences_presence_etablissements.csv"))
  write_utf8_csv(matrix, file.path(output, "matrice_transition_presence.csv"))
  write_utf8_csv(profiles, file.path(output, "profils_sequences_presence.csv"))
  write_utf8_csv(registers, file.path(output, "registres_interpretation.csv"))
  write_utf8_csv(validation, file.path(output, "validation_trajectoires.csv"))
  list(feasibility = feasibility, sequences = sequences, matrix = matrix, profiles = profiles,
       registers = registers, validation = validation, figures = figures)
}
