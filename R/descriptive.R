# Analyse descriptive des agrégats publics, sans interprétation causale.

sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

composition_long <- function(data, total_variable, component_variables, labels, degree, dimension) {
  pieces <- lapply(seq_along(component_variables), function(i) {
    variable <- component_variables[[i]]
    data.table::as.data.table(data)[, {
      observed <- !is.na(get(variable)) & !is.na(get(total_variable))
      denominator <- sum(get(total_variable)[observed], na.rm = TRUE)
      numerator <- sum(get(variable)[observed], na.rm = TRUE)
      list(etp = numerator, etp_denominateur = denominator,
           proportion = if (denominator > 0) numerator / denominator else NA_real_,
           couverture_etp = if (sum(get(total_variable), na.rm = TRUE) > 0)
             denominator / sum(get(total_variable), na.rm = TRUE) else NA_real_)
    }, by = annee][, `:=`(degre = degree, dimension = dimension, classe = labels[[i]])]
  })
  as.data.frame(data.table::rbindlist(pieces, use.names = TRUE))
}

descriptive_plot_labels <- function(title, field, period, unit, source, note = NULL) {
  ggplot2::labs(
    title = title,
    subtitle = paste0("Champ : ", field, " | Période : ", period, " | Unité : ", unit),
    caption = paste0("Source : ", source, if (!is.null(note)) paste0("\nNote de lecture : ", note) else "")
  )
}

descriptive_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.title.position = "plot", plot.caption.position = "plot",
                   legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())
}

build_descriptive_tables <- function(root = here::here()) {
  clean <- file.path(root, "data", "interim", "clean")
  features <- file.path(root, "data", "processed", "features")
  panel_dir <- file.path(root, "data", "processed", "panel")
  p1 <- data.table::as.data.table(readRDS(file.path(clean, "personnels_1d_clean.rds")))
  p2 <- data.table::as.data.table(readRDS(file.path(clean, "personnels_2d_clean.rds")))
  f1 <- data.table::as.data.table(readRDS(file.path(features, "personnels_1d_features.rds")))
  f2 <- data.table::as.data.table(readRDS(file.path(features, "personnels_2d_features.rds")))
  mobility <- data.table::as.data.table(readRDS(file.path(features, "mobilite_1d_features.rds")))
  panel <- data.table::as.data.table(readRDS(file.path(panel_dir, "etablissement_annee_panel.rds")))

  effectifs <- data.table::rbindlist(list(
    p1[, .(etablissements = data.table::uniqueN(uai), etp_enseignants = sum_or_na(etp_d_enseignants_hommes_et_femmes)), by = annee][, degre := "Premier degré"],
    p2[, .(etablissements = data.table::uniqueN(uai), etp_enseignants = sum_or_na(etp_enseignants_hommes_et_femmes)), by = annee][, degre := "Second degré"]
  ), use.names = TRUE)
  data.table::setorder(effectifs, degre, annee)
  effectifs[, `:=`(
    variation_etp = etp_enseignants - data.table::shift(etp_enseignants),
    taux_variation_etp = (etp_enseignants / data.table::shift(etp_enseignants)) - 1
  ), by = degre]

  age <- data.table::rbindlist(list(
    composition_long(p1, "etp_d_enseignants_hommes_et_femmes",
      c("etp_d_enseignants_de_moins_de_35_ans", "etp_d_enseignants_de_35_a_moins_de_50_ans", "etp_d_enseignants_de_50_ans_ou_plus"),
      c("Moins de 35 ans", "35 à 49 ans", "50 ans ou plus"), "Premier degré", "Âge"),
    composition_long(p2, "etp_enseignants_hommes_et_femmes",
      c("etp_d_enseignants_de_moins_de_35_ans", "etp_d_enseignants_de_35_a_moins_de_50_ans", "etp_d_enseignants_de_50_ans_ou_plus"),
      c("Moins de 35 ans", "35 à 49 ans", "50 ans ou plus"), "Second degré", "Âge")
  ), use.names = TRUE)

  tenure_variables <- c(
    "etp_d_enseignants_ayant_une_anciennete_dans_l_etablissement_de_moins_de_2_ans",
    "etp_d_enseignants_ayant_une_anciennete_dans_l_etablissement_de_2_ans_a_moins_de_5_ans",
    "etp_d_enseignants_ayant_une_anciennete_dans_l_etablissement_de_5_ans_a_moins_de_8_ans",
    "etp_d_enseignants_ayant_une_anciennete_dans_l_etablissement_de_8_ans_ou_plus")
  tenure_labels <- c("Moins de 2 ans", "2 à 4 ans", "5 à 7 ans", "8 ans ou plus")
  tenure <- data.table::rbindlist(list(
    composition_long(p1, "etp_d_enseignants_hommes_et_femmes", tenure_variables, tenure_labels,
                     "Premier degré", "Ancienneté établissement"),
    composition_long(p2, "etp_enseignants_hommes_et_femmes", tenure_variables, tenure_labels,
                     "Second degré", "Ancienneté établissement")
  ), use.names = TRUE)

  corps_variables <- c("etp_d_enseignants_agreges", "etp_d_enseignants_certifies_peps", "etp_d_enseignants_plp",
                       "etp_d_enseignants_titulaires_d_un_autre_corps", "etp_d_enseignants_non_titulaires")
  corps_labels <- c("Agrégés", "Certifiés et PEPS", "PLP", "Autres corps titulaires", "Non-titulaires")
  corps <- data.table::rbindlist(lapply(seq_along(corps_variables), function(i) {
    variable <- corps_variables[[i]]
    p2[, .(etp = sum_or_na(get(variable))), by = annee][, corps := corps_labels[[i]]]
  }))
  corps[, proportion := etp / sum(etp, na.rm = TRUE), by = annee]

  establishments <- data.table::rbindlist(list(
    p1[, as.list(stats::quantile(etp_d_enseignants_hommes_et_femmes, c(0, .25, .5, .75, .9, 1), na.rm = TRUE,
                                  names = FALSE)), by = annee][, degre := "Premier degré"],
    p2[, as.list(stats::quantile(etp_enseignants_hommes_et_femmes, c(0, .25, .5, .75, .9, 1), na.rm = TRUE,
                                  names = FALSE)), by = annee][, degre := "Second degré"]
  ), fill = TRUE)
  data.table::setnames(establishments, c("annee", "V1", "V2", "V3", "V4", "V5", "V6", "degre"),
                       c("annee", "minimum", "q1", "mediane", "q3", "p90", "maximum", "degre"))

  territories <- p1[, .(etablissements = data.table::uniqueN(uai),
                         etp_enseignants = sum_or_na(etp_d_enseignants_hommes_et_femmes)),
                    by = .(annee, code_departement, nom_departement)]

  ep <- data.table::rbindlist(list(
    f1[appariement_annuaire == TRUE, .(etablissements = .N, etp_enseignants = sum_or_na(etp_enseignants)),
       by = .(annee, education_prioritaire)][, degre := "Premier degré"],
    f2[appariement_annuaire == TRUE, .(etablissements = .N, etp_enseignants = sum_or_na(etp_enseignants)),
       by = .(annee, education_prioritaire)][, degre := "Second degré"]
  ), use.names = TRUE)
  ep[, part_etp := etp_enseignants / sum(etp_enseignants, na.rm = TRUE), by = .(degre, annee)]
  ep[, groupe := ifelse(education_prioritaire, "REP ou REP+", "Hors EP (snapshot)")]

  mobility_long <- data.table::melt(mobility,
    id.vars = c("annee", "code_departement", "nom_departement"),
    measure.vars = c("pression_demandes_entree_sortie", "ratio_sorties_realisees_demandees", "ratio_entrees_sorties_realisees"),
    variable.name = "indicateur", value.name = "valeur")
  mobility_summary <- mobility_long[, .(departements_observes = sum(!is.na(valeur)), moyenne = mean(valeur, na.rm = TRUE),
    q1 = stats::quantile(valeur, .25, na.rm = TRUE), mediane = stats::median(valeur, na.rm = TRUE),
    q3 = stats::quantile(valeur, .75, na.rm = TRUE), minimum = min(valeur, na.rm = TRUE), maximum = max(valeur, na.rm = TRUE)),
    by = .(annee, indicateur)]

  observation_status <- panel[, .N, by = statut_observation]
  grade <- data.frame(variable = "grade_individuel", disponibilite = "NON DISPONIBLE",
                      justification = "Aucune variable de grade individuel dans les données publiques acquises.")

  list(effectifs = as.data.frame(effectifs), age = as.data.frame(age), anciennete = as.data.frame(tenure),
       corps = as.data.frame(corps), grade = grade, etablissements = as.data.frame(establishments),
       territoires = as.data.frame(territories), education_prioritaire = as.data.frame(ep),
       mobilite = as.data.frame(mobility_long), mobilite_resume = as.data.frame(mobility_summary),
       presence_panel = as.data.frame(observation_status))
}

build_descriptive_plots <- function(tables, root = here::here()) {
  output <- file.path(root, "outputs", "figures", "descriptive")
  fs::dir_create(output)
  plots <- list()
  plots$effectifs <- ggplot2::ggplot(tables$effectifs, ggplot2::aes(annee, etp_enseignants, color = degre, group = degre)) +
    ggplot2::geom_line(linewidth = .8, na.rm = TRUE) + ggplot2::geom_point(size = 2.5) +
    descriptive_plot_labels("Effectifs enseignants en équivalent temps plein", "Établissements scolaires couverts par les jeux de personnels",
      "2024–2025 selon le degré", "ETP enseignants", "DEPP, indicateurs de personnels 1D et 2D",
      "Le second degré ne comporte qu'un millésime; aucun taux d'évolution n'y est calculé.") + descriptive_theme()

  plots$age <- ggplot2::ggplot(tables$age, ggplot2::aes(factor(annee), proportion, fill = classe)) +
    ggplot2::geom_col() + ggplot2::facet_wrap(~degre) +
    descriptive_plot_labels("Composition des ETP enseignants par classe d'âge", "Personnel enseignant agrégé par établissement",
      "2024–2025 selon le degré", "Proportion d'ETP", "DEPP, indicateurs de personnels 1D et 2D",
      "Les proportions décrivent des établissements agrégés et non des probabilités individuelles.") +
    ggplot2::labs(x = "Année", y = "Proportion", fill = "Classe d'âge") + descriptive_theme()

  plots$anciennete <- ggplot2::ggplot(tables$anciennete, ggplot2::aes(factor(annee), proportion, fill = classe)) +
    ggplot2::geom_col() + ggplot2::facet_wrap(~degre) +
    descriptive_plot_labels("Ancienneté des enseignants dans l'établissement", "Personnel enseignant agrégé par établissement",
      "2024–2025 selon le degré", "Proportion d'ETP", "DEPP, indicateurs de personnels 1D et 2D",
      "Il s'agit d'ancienneté dans l'établissement, pas d'ancienneté de carrière.") +
    ggplot2::labs(x = "Année", y = "Proportion", fill = "Ancienneté") + descriptive_theme()

  plots$corps <- ggplot2::ggplot(tables$corps, ggplot2::aes(stats::reorder(corps, etp), etp)) +
    ggplot2::geom_col(fill = "#35608D") + ggplot2::coord_flip() +
    descriptive_plot_labels("Répartition agrégée des ETP enseignants par groupe de corps", "Établissements du second degré",
      "2024", "ETP enseignants", "DEPP, indicateurs de personnels 2D",
      "Les groupes de corps sont agrégés; le grade individuel n'est pas disponible.") +
    ggplot2::labs(x = NULL, y = "ETP") + descriptive_theme()

  p1_raw <- readRDS(file.path(root, "data", "interim", "clean", "personnels_1d_clean.rds"))
  p2_raw <- readRDS(file.path(root, "data", "interim", "clean", "personnels_2d_clean.rds"))
  establishment_values <- data.table::rbindlist(list(
    data.table::data.table(annee = p1_raw$annee, degre = "Premier degré", etp = p1_raw$etp_d_enseignants_hommes_et_femmes),
    data.table::data.table(annee = p2_raw$annee, degre = "Second degré", etp = p2_raw$etp_enseignants_hommes_et_femmes)))
  establishment_values <- establishment_values[!is.na(etp)]
  plots$etablissements <- ggplot2::ggplot(establishment_values, ggplot2::aes(factor(annee), etp, fill = degre)) +
    ggplot2::geom_boxplot(outlier.alpha = .08) + ggplot2::facet_wrap(~degre, scales = "free_y") +
    descriptive_plot_labels("Distribution des effectifs enseignants par établissement", "Établissements avec indicateurs de personnels",
      "2024–2025 selon le degré", "ETP par établissement", "DEPP, indicateurs de personnels 1D et 2D",
      "La boîte décrit la médiane et les quartiles; les échelles diffèrent selon le degré.") +
    ggplot2::labs(x = "Année", y = "ETP") + descriptive_theme() + ggplot2::theme(legend.position = "none")

  territorial_latest <- tables$territoires[tables$territoires$annee == max(tables$territoires$annee), ]
  territorial_latest <- head(territorial_latest[order(-territorial_latest$etp_enseignants), ], 15)
  plots$territoires <- ggplot2::ggplot(territorial_latest,
    ggplot2::aes(stats::reorder(nom_departement, etp_enseignants), etp_enseignants)) +
    ggplot2::geom_col(fill = "#4E8F69") + ggplot2::coord_flip() +
    descriptive_plot_labels("Départements comptant le plus d'ETP enseignants du premier degré", "Quinze premiers départements",
      as.character(max(territorial_latest$annee)), "ETP enseignants", "DEPP, indicateurs de personnels 1D",
      "Un effectif territorial n'est ni un taux ni une mesure d'attractivité.") +
    ggplot2::labs(x = NULL, y = "ETP") + descriptive_theme()

  plots$education_prioritaire <- ggplot2::ggplot(tables$education_prioritaire,
    ggplot2::aes(factor(annee), part_etp, fill = groupe)) + ggplot2::geom_col() + ggplot2::facet_wrap(~degre) +
    descriptive_plot_labels("Répartition des ETP selon l'éducation prioritaire", "UAI appariées à l'annuaire de l'éducation",
      "Personnels 2024–2025; statut annuaire 2026", "Proportion d'ETP", "DEPP et Annuaire de l'éducation",
      "Le statut EP est un snapshot récent et ne garantit pas le statut historique exact.") +
    ggplot2::labs(x = "Année", y = "Proportion", fill = "Groupe") + descriptive_theme()

  plots$mobilite <- ggplot2::ggplot(tables$mobilite, ggplot2::aes(indicateur, valeur)) +
    ggplot2::geom_boxplot(fill = "#C97A40") + ggplot2::coord_flip() +
    descriptive_plot_labels("Distribution départementale des ratios du mouvement 1D public", "Départements publiés",
      "2019", "Ratio départemental", "DGRH-DEPP, mouvement interdépartemental du premier degré public",
      "Ces ratios ne sont pas des taux individuels de mobilité et les effectifs bruts ne sont pas publiés dans ce fichier.") +
    ggplot2::labs(x = NULL, y = "Ratio") + descriptive_theme()

  paths <- vapply(names(plots), function(name) {
    path <- file.path(output, paste0(name, ".png"))
    ggplot2::ggsave(path, plots[[name]], width = 9, height = 5.5, dpi = 160, bg = "white")
    path
  }, character(1))
  paths
}

validate_descriptive_outputs <- function(tables, figures) {
  checks <- data.frame(
    INDICATEUR = c("Effectifs non négatifs", "Proportions d'âge bornées", "Proportions d'ancienneté bornées",
                   "Clé territoires", "Figures produites", "Grade individuel non inventé"),
    RESULTAT = c(
      sum(tables$effectifs$etp_enseignants < 0, na.rm = TRUE),
      sum(tables$age$proportion < 0 | tables$age$proportion > 1, na.rm = TRUE),
      sum(tables$anciennete$proportion < 0 | tables$anciennete$proportion > 1, na.rm = TRUE),
      sum(duplicated(tables$territoires[c("annee", "code_departement")])),
      sum(!file.exists(figures)),
      sum(tables$grade$disponibilite != "NON DISPONIBLE")),
    SEUIL = 0, stringsAsFactors = FALSE)
  checks$STATUT <- ifelse(checks$RESULTAT == checks$SEUIL, "OK", "ERREUR")
  checks$ACTION <- ifelse(checks$STATUT == "OK", "Aucune", "Corriger avant diffusion")
  checks
}

run_descriptive_analysis <- function(panel_data = NULL, root = here::here()) {
  tables <- build_descriptive_tables(root)
  figures <- build_descriptive_plots(tables, root)
  validation <- validate_descriptive_outputs(tables, figures)
  if (any(validation$STATUT == "ERREUR")) stop("Validation descriptive échouée.", call. = FALSE)
  output <- file.path(root, "outputs", "tables", "descriptive")
  fs::dir_create(output)
  for (name in names(tables)) write_utf8_csv(tables[[name]], file.path(output, paste0(name, ".csv")))
  write_utf8_csv(validation, file.path(output, "validation_descriptive.csv"))
  list(tables = tables, figures = figures, validation = validation)
}
