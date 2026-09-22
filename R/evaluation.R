# Evaluation comparative des modeles, conditionnelle a des predictions hors echantillon.

evaluation_metric_definitions <- function() {
  data.frame(
    METRIQUE = c("ROC-AUC", "PR-AUC", "Precision", "Recall", "F1", "Sensibilite", "Specificite",
      "Accuracy", "Matrice de confusion", "Calibration"),
    DEFINITION = c("Classement global entre positifs et negatifs", "Compromis precision-rappel centre sur la classe positive rare",
      "Part des alertes positives qui sont effectivement positives", "Part des positifs retrouves",
      "Moyenne harmonique de la precision et du recall", "Equivalent du recall pour la classe positive",
      "Part des negatifs correctement identifies", "Part totale de classifications correctes; secondaire si la cible est rare",
      "Nombres de vrais/faux positifs et vrais/faux negatifs", "Accord entre probabilites predites et frequences observees"),
    USAGE = c("Secondaire", "Principal", "Selon le cout des faux positifs", "Selon le cout des faux negatifs",
      "Equilibre precision-rappel", "Selon le cout des faux negatifs", "Controle des fausses alertes",
      "Jamais seule", "Toujours avec le seuil documente", "Brier, ECE et courbe par classes de risque"),
    stringsAsFactors = FALSE)
}

evaluation_data_contract <- function() {
  data.frame(
    VARIABLE = c("modele", "periode", "ensemble", "verite", ".pred_oui"),
    DEFINITION = c("Nom stable du workflow", "Annee d'origine du fold ou du test", "validation temporelle ou test final",
      "Cible observee non/oui", "Probabilite predite de oui"),
    REGLE = c("Non manquant", "Ordre temporel explicite", "Jamais train", "Facteur niveaux non puis oui", "Entre 0 et 1"),
    stringsAsFactors = FALSE)
}

validate_evaluation_predictions <- function(predictions) {
  required <- evaluation_data_contract()$VARIABLE
  missing <- setdiff(required, names(predictions))
  if (length(missing)) stop("Variables d'evaluation manquantes: ", paste(missing, collapse = ", "), call. = FALSE)
  if (anyNA(predictions[, required])) stop("Valeurs manquantes dans les variables d'evaluation.", call. = FALSE)
  if (any(predictions$.pred_oui < 0 | predictions$.pred_oui > 1)) stop("Les probabilites doivent etre comprises entre 0 et 1.", call. = FALSE)
  truth <- as.character(predictions$verite)
  if (any(!truth %in% c("non", "oui"))) stop("La verite doit utiliser les modalites non/oui.", call. = FALSE)
  if (any(predictions$ensemble == "train")) stop("Les performances d'entrainement ne sont pas des performances de generalisation.", call. = FALSE)
  invisible(TRUE)
}

metric_value <- function(fun, truth, estimate = NULL, probability = NULL) {
  if (!is.null(probability)) fun(truth = truth, estimate = probability, event_level = "second")
  else fun(truth = truth, estimate = estimate, event_level = "second")
}

classification_metrics_by_period <- function(predictions, threshold = .5) {
  validate_evaluation_predictions(predictions)
  data <- predictions
  data$verite <- factor(as.character(data$verite), levels = c("non", "oui"))
  data$.pred_class <- factor(ifelse(data$.pred_oui >= threshold, "oui", "non"), levels = c("non", "oui"))
  groups <- split(data, interaction(data$modele, data$periode, data$ensemble, drop = TRUE))
  do.call(rbind, lapply(groups, function(x) data.frame(
    modele = x$modele[1], periode = x$periode[1], ensemble = x$ensemble[1], seuil = threshold,
    roc_auc = metric_value(yardstick::roc_auc_vec, x$verite, probability = x$.pred_oui),
    pr_auc = metric_value(yardstick::pr_auc_vec, x$verite, probability = x$.pred_oui),
    precision = metric_value(yardstick::precision_vec, x$verite, estimate = x$.pred_class),
    recall = metric_value(yardstick::recall_vec, x$verite, estimate = x$.pred_class),
    f1 = metric_value(yardstick::f_meas_vec, x$verite, estimate = x$.pred_class),
    sensibilite = metric_value(yardstick::sens_vec, x$verite, estimate = x$.pred_class),
    specificite = metric_value(yardstick::spec_vec, x$verite, estimate = x$.pred_class),
    accuracy = yardstick::accuracy_vec(x$verite, x$.pred_class), observations = nrow(x), prevalence = mean(x$verite == "oui"),
    stringsAsFactors = FALSE)))
}

confusion_matrices <- function(predictions, threshold = .5) {
  validate_evaluation_predictions(predictions)
  data <- predictions
  data$verite <- factor(as.character(data$verite), levels = c("non", "oui"))
  data$.pred_class <- factor(ifelse(data$.pred_oui >= threshold, "oui", "non"), levels = c("non", "oui"))
  groups <- split(data, interaction(data$modele, data$periode, data$ensemble, drop = TRUE))
  do.call(rbind, lapply(groups, function(x) {
    table <- as.data.frame(table(verite = x$verite, prediction = x$.pred_class), stringsAsFactors = FALSE)
    table$modele <- x$modele[1]; table$periode <- x$periode[1]; table$ensemble <- x$ensemble[1]; table$seuil <- threshold
    table[, c("modele", "periode", "ensemble", "seuil", "verite", "prediction", "Freq")]
  }))
}

calibration_by_period <- function(predictions, bins = 10L) {
  validate_evaluation_predictions(predictions)
  data <- predictions; data$issue <- as.integer(as.character(data$verite) == "oui")
  groups <- split(data, interaction(data$modele, data$periode, data$ensemble, drop = TRUE))
  curves <- do.call(rbind, lapply(groups, function(x) {
    number <- min(bins, length(unique(x$.pred_oui)))
    breaks <- unique(stats::quantile(x$.pred_oui, seq(0, 1, length.out = number + 1), na.rm = TRUE))
    x$classe_risque <- cut(x$.pred_oui, breaks = breaks, include.lowest = TRUE, ordered_result = TRUE)
    out <- data.table::as.data.table(x)[, .(observations = .N, probabilite_moyenne = mean(.pred_oui), frequence_observee = mean(issue)), by = classe_risque]
    out[, `:=`(modele = x$modele[1], periode = x$periode[1], ensemble = x$ensemble[1])]
    as.data.frame(out)
  }))
  summaries <- data.table::as.data.table(curves)[, .(
    brier = sum(observations * (frequence_observee - probabilite_moyenne)^2) / sum(observations),
    ece = sum(observations * abs(frequence_observee - probabilite_moyenne)) / sum(observations)),
    by = .(modele, periode, ensemble)]
  list(curves = curves, summaries = as.data.frame(summaries))
}

model_stability <- function(metrics) {
  validation <- data.table::as.data.table(metrics)[ensemble == "validation"]
  if (!nrow(validation)) return(data.frame())
  validation[, .(folds = .N, pr_auc_moyenne = mean(pr_auc), pr_auc_ecart_type = stats::sd(pr_auc),
    pr_auc_min = min(pr_auc), pr_auc_max = max(pr_auc), roc_auc_moyenne = mean(roc_auc),
    f1_moyenne = mean(f1), f1_ecart_type = stats::sd(f1)), by = modele] |> as.data.frame()
}

model_generalization <- function(metrics) {
  dt <- data.table::as.data.table(metrics)
  validation <- dt[ensemble == "validation", .(pr_auc_validation = mean(pr_auc), roc_auc_validation = mean(roc_auc)), by = modele]
  test <- dt[ensemble == "test", .(pr_auc_test = mean(pr_auc), roc_auc_test = mean(roc_auc)), by = modele]
  result <- merge(validation, test, by = "modele", all = TRUE)
  result$ecart_pr_auc_test_validation <- result$pr_auc_test - result$pr_auc_validation
  result$ecart_roc_auc_test_validation <- result$roc_auc_test - result$roc_auc_validation
  as.data.frame(result)
}

model_characteristics <- function() {
  data.frame(
    modele = c("regression_logistique", "random_forest", "gradient_boosting"),
    complexite = c("Faible a moderee", "Elevee", "Elevee"),
    interpretabilite = c("Elevee: coefficients conditionnels", "Moyenne a faible: importance et explications locales",
      "Moyenne a faible: importance et explications locales"),
    vigilance = c("Linearity sur l'echelle du logit", "Cout calculatoire et surapprentissage", "Tuning sensible et surapprentissage"),
    stringsAsFactors = FALSE)
}

evaluation_decision_rules <- function() {
  data.frame(
    CRITERE = c("Performance", "Stabilite", "Generalisation", "Calibration", "Complexite", "Interpretabilite", "Decision"),
    REGLE = c("PR-AUC prioritaire, ROC-AUC et metriques au seuil en complement", "Examiner moyenne, dispersion et pire periode",
      "Comparer validation temporelle au test final", "Examiner Brier, ECE et courbe; recalibrer si necessaire",
      "Preferer le modele plus simple si le gain complexe est faible ou instable", "Adapter au public et a l'usage decisionnel",
      "Revue multicritere documentee; aucune selection automatique au maximum d'une metrique"),
    stringsAsFactors = FALSE)
}

evaluate_model_predictions <- function(predictions, threshold = .5) {
  metrics <- classification_metrics_by_period(predictions, threshold)
  calibration <- calibration_by_period(predictions)
  list(metrics = metrics, confusion = confusion_matrices(predictions, threshold), calibration = calibration,
    stability = model_stability(metrics), generalization = model_generalization(metrics),
    characteristics = model_characteristics(), rules = evaluation_decision_rules())
}

run_model_evaluation <- function(modeling_results, root = here::here()) {
  evaluable <- !is.null(modeling_results$trained_models) && !is.null(modeling_results$metrics)
  status <- if (evaluable) "EVALUABLE" else "NON EVALUABLE - AUCUNE PREDICTION HORS ECHANTILLON"
  output <- file.path(root, "outputs", "tables", "evaluation"); fs::dir_create(output)
  write_utf8_csv(evaluation_metric_definitions(), file.path(output, "definition_metriques.csv"))
  write_utf8_csv(evaluation_data_contract(), file.path(output, "contrat_predictions.csv"))
  write_utf8_csv(model_characteristics(), file.path(output, "caracteristiques_modeles.csv"))
  write_utf8_csv(evaluation_decision_rules(), file.path(output, "regles_decision.csv"))
  data.frame(OBJET = "Comparaison empirique", STATUT = status,
    RAISON = "L'etape 13 n'a entraine aucun modele faute de cible individuelle et de profondeur temporelle") |>
    write_utf8_csv(file.path(output, "statut_evaluation.csv"))
  list(status = status, definitions = evaluation_metric_definitions(), contract = evaluation_data_contract(),
    characteristics = model_characteristics(), rules = evaluation_decision_rules(), comparison = NULL)
}
