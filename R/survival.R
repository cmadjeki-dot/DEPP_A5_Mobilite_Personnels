# Analyse de survie: contrat scientifique et execution conditionnelle.

survival_scientific_definition <- function() {
  data.frame(
    ELEMENT = c("Question", "Unite", "Origine temporelle", "Duree", "Evenement", "Censure a droite", "Risques concurrents"),
    DEFINITION = c(
      "Combien de temps s'ecoule avant la premiere mobilite professionnelle observee ?",
      "Personnel identifie de facon stable dans le temps",
      "Date d'entree dans le champ d'observation ou date de prise de poste, definie avant l'evenement",
      "Temps entre l'origine et la premiere mobilite; sinon entre l'origine et la derniere date d'observation",
      "Premier changement d'etablissement observe et date; variantes departement, academie ou fonction a definir separement",
      "Personnel sans mobilite observee a la derniere date fiable, toujours present dans le champ",
      "Sortie du champ, retraite et deces ne doivent pas etre codes comme mobilite; une analyse specifique peut etre requise"),
    stringsAsFactors = FALSE)
}

survival_data_contract <- function() {
  data.frame(
    VARIABLE = c("id_personnel", "date_origine", "date_fin", "evenement_mobilite", "groupe", "age_origine", "anciennete_origine"),
    ROLE = c("Identifiant longitudinal", "Origine temporelle", "Evenement ou censure", "Statut 1/0", "Comparaison Kaplan-Meier",
      "Controle Cox", "Controle Cox"),
    OBLIGATOIRE = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
    REGLE = c("Unique par personne dans la table analytique", "Date non manquante et anterieure a date_fin",
      "Date non manquante", "1 si mobilite observee, 0 si censure", "Defini a l'origine, jamais apres l'evenement",
      "Mesure a l'origine", "Mesure a l'origine"),
    stringsAsFactors = FALSE)
}

audit_survival_feasibility <- function(panel) {
  required <- survival_data_contract()$VARIABLE[survival_data_contract()$OBLIGATOIRE]
  available <- required %in% names(panel)
  checks <- data.frame(
    CONTROLE = c("Identifiant personnel stable", "Date d'origine individuelle", "Date d'evenement ou censure",
      "Evenement de mobilite individuel", "Au moins trois dates possibles", "Disparition distinguee d'une sortie"),
    REQUIS = c("id_personnel", "date_origine", "date_fin", "evenement_mobilite", ">= 3 vagues", "motif/statut de sortie"),
    RESULTAT = c(ifelse(available[1], "Disponible", "Absent"), ifelse(available[2], "Disponible", "Absent"),
      ifelse(available[3], "Disponible", "Absent"), ifelse(available[4], "Disponible", "Absent"),
      paste0(length(unique(panel$annee)), " vagues"), "Non"),
    STATUT = c(ifelse(available[1:4], "OK", "BLOQUANT"),
      ifelse(length(unique(panel$annee)) >= 3, "OK", "BLOQUANT"), "BLOQUANT"),
    ACTION = c(rep("Obtenir les donnees longitudinales internes DEPP", 4),
      "Etendre la fenetre temporelle", "Obtenir un statut de sortie fiable"),
    stringsAsFactors = FALSE)
  checks
}

validate_survival_input <- function(data) {
  contract <- survival_data_contract()
  required <- contract$VARIABLE[contract$OBLIGATOIRE]
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("Variables de survie manquantes: ", paste(missing, collapse = ", "), call. = FALSE)
  if (anyDuplicated(data$id_personnel)) stop("La table analytique doit contenir une ligne par personnel.", call. = FALSE)
  if (any(is.na(data[, required]))) stop("Valeurs manquantes dans les variables obligatoires.", call. = FALSE)
  if (any(!data$evenement_mobilite %in% c(0, 1))) stop("L'evenement doit etre code 0/1.", call. = FALSE)
  origin <- as.Date(data$date_origine); end <- as.Date(data$date_fin)
  if (any(end <= origin)) stop("Chaque date_fin doit etre posterieure a date_origine.", call. = FALSE)
  invisible(TRUE)
}

prepare_personnel_survival <- function(data) {
  validate_survival_input(data)
  result <- data
  result$date_origine <- as.Date(result$date_origine)
  result$date_fin <- as.Date(result$date_fin)
  result$duree_annees <- as.numeric(result$date_fin - result$date_origine) / 365.25
  result$evenement_mobilite <- as.integer(result$evenement_mobilite)
  result$objet_survie <- survival::Surv(result$duree_annees, result$evenement_mobilite)
  result
}

fit_personnel_survival <- function(data) {
  prepared <- prepare_personnel_survival(data)
  controls <- intersect(c("age_origine", "anciennete_origine"), names(prepared))
  rhs <- paste(c("groupe", controls), collapse = " + ")
  cox_formula <- stats::as.formula(paste("survival::Surv(duree_annees, evenement_mobilite) ~", rhs))
  km_formula <- survival::Surv(duree_annees, evenement_mobilite) ~ groupe
  km <- survival::survfit(km_formula, data = prepared, conf.type = "log-log")
  comparison <- survival::survdiff(km_formula, data = prepared)
  cox <- survival::coxph(cox_formula, data = prepared, ties = "efron", x = TRUE, model = TRUE)
  proportional_hazards <- survival::cox.zph(cox)
  coefficients <- broom::tidy(cox, exponentiate = TRUE, conf.int = TRUE, conf.level = .95)
  list(data = prepared, kaplan_meier = km, logrank = comparison, cox = cox,
    hazard_ratios = coefficients, proportional_hazards = proportional_hazards)
}

survival_output_register <- function(feasibility) {
  blocked <- any(feasibility$STATUT == "BLOQUANT")
  data.frame(
    SORTIE = c("Objet Surv", "Kaplan-Meier", "Comparaisons log-rank", "Modele de Cox", "Hazard ratios et IC95 %", "Test des risques proportionnels"),
    METHODE = c("Surv()", "survfit()", "survdiff()", "coxph()", "broom::tidy(coxph, exponentiate=TRUE)", "cox.zph()"),
    PRODUITE = if (blocked) "NON" else "OUI",
    RAISON = if (blocked) "Mobilite individuelle et temps jusqu'a l'evenement non observables dans les donnees ouvertes" else "Donnees conformes au contrat",
    stringsAsFactors = FALSE)
}

survival_interpretation_rules <- function() {
  data.frame(
    OBJET = c("Courbe Kaplan-Meier", "Hazard ratio > 1", "Hazard ratio < 1", "Toutes choses egales par ailleurs", "Causalite"),
    LANGAGE_ACCESSIBLE = c(
      "Proportion estimee de personnels n'ayant pas encore connu la mobilite definie au fil du temps",
      "La mobilite instantanee est plus frequente dans le groupe compare, conditionnellement aux variables du modele",
      "La mobilite instantanee est moins frequente dans le groupe compare, conditionnellement aux variables du modele",
      "Comparaison a valeurs identiques des controles observes inclus; les facteurs non observes restent possibles",
      "Un modele de Cox observationnel n'identifie pas a lui seul un effet causal"),
    stringsAsFactors = FALSE)
}

run_survival_analysis <- function(panel_data = NULL, root = here::here()) {
  panel <- readRDS(file.path(root, "data", "processed", "panel", "etablissement_annee_panel.rds"))
  feasibility <- audit_survival_feasibility(panel)
  outputs <- survival_output_register(feasibility)
  output <- file.path(root, "outputs", "tables", "survival"); fs::dir_create(output)
  write_utf8_csv(survival_scientific_definition(), file.path(output, "definition_scientifique.csv"))
  write_utf8_csv(survival_data_contract(), file.path(output, "contrat_donnees.csv"))
  write_utf8_csv(feasibility, file.path(output, "audit_faisabilite.csv"))
  write_utf8_csv(outputs, file.path(output, "registre_sorties.csv"))
  write_utf8_csv(survival_interpretation_rules(), file.path(output, "regles_interpretation.csv"))
  list(status = "NON IDENTIFIABLE AVEC L'OPEN DATA", definition = survival_scientific_definition(),
    contract = survival_data_contract(), feasibility = feasibility, outputs = outputs,
    interpretation = survival_interpretation_rules(), models = NULL)
}
