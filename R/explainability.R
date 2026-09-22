# Explicabilite conditionnelle des modeles predictifs.

explainability_definitions <- function() {
  data.frame(
    OBJET = c("Importance globale", "Importance par permutation", "Dependance partielle", "SHAP", "Coefficient econometrique", "Effet causal"),
    QUESTION = c("Quelles variables contribuent le plus au modele dans son ensemble ?",
      "Quelle perte de performance apparait lorsque les valeurs d'une variable sont melangees ?",
      "Comment la prediction moyenne evolue lorsque la variable varie artificiellement ?",
      "Comment les variables contribuent a une prediction, relativement a une reference ?",
      "Quelle association conditionnelle est estimee dans un modele parametrique ?",
      "Que changerait une intervention sur la variable ?"),
    INTERPRETATION = c("Predictive, relative au modele et aux donnees", "Predictive; sensible aux variables correlees",
      "Predictive; peut extrapoler vers des combinaisons peu plausibles", "Predictive; depend de la reference et des dependances",
      "Associationnelle, sous specification et controles observes", "Necessite une strategie d'identification causale"),
    stringsAsFactors = FALSE)
}

explainability_data_contract <- function() {
  data.frame(
    ELEMENT = c("Modele", "X de reference", "Y", "Fonction de prediction", "Jeu d'explication", "Provenance temporelle"),
    REGLE = c("Workflow final ajuste sans le test pendant le tuning", "Predicteurs exactement compatibles avec le modele",
      "Cible observee pour la permutation", "Retourne P(mobilite = oui)", "Echantillon representatif et taille documentee",
      "Test final ou jeu futur; aucune variable posterieure a t"),
    stringsAsFactors = FALSE)
}

validate_explainability_inputs <- function(model, x, y, predict_function) {
  if (is.null(model)) stop("Modele absent.", call. = FALSE)
  if (!is.data.frame(x) || !nrow(x) || !ncol(x)) stop("X doit etre un data.frame non vide.", call. = FALSE)
  if (length(y) != nrow(x)) stop("Y et X doivent avoir le meme nombre de lignes.", call. = FALSE)
  if (!is.function(predict_function)) stop("predict_function doit etre une fonction.", call. = FALSE)
  predictions <- predict_function(model, x[seq_len(min(3L, nrow(x))), , drop = FALSE])
  if (!is.numeric(predictions) || length(predictions) != min(3L, nrow(x)) || any(predictions < 0 | predictions > 1))
    stop("La fonction de prediction doit retourner une probabilite par ligne.", call. = FALSE)
  invisible(TRUE)
}

build_dalex_explainer <- function(model, x, y, predict_function, label) {
  validate_explainability_inputs(model, x, y, predict_function)
  DALEX::explain(model = model, data = x, y = as.numeric(as.character(y) %in% c("1", "oui")),
    predict_function = predict_function, label = label, verbose = FALSE)
}

compute_global_importance <- function(model, x, y, predict_function, label, repetitions = 20L) {
  train <- x; train$.outcome <- factor(ifelse(as.character(y) %in% c("1", "oui"), "oui", "non"), levels = c("non", "oui"))
  vip_importance <- vip::vi_permute(model, feature_names = names(x), train = train, target = ".outcome",
    metric = "roc_auc", event_level = "second", pred_wrapper = predict_function,
    nsim = repetitions, smaller_is_better = FALSE)
  explainer <- build_dalex_explainer(model, x, y, predict_function, label)
  dalex_importance <- DALEX::model_parts(explainer, type = "difference", B = repetitions,
    loss_function = DALEX::loss_one_minus_auc)
  list(vip_permutation = as.data.frame(vip_importance), dalex_permutation = as.data.frame(dalex_importance))
}

compute_partial_dependence <- function(model, x, y, predict_function, label, variables = names(x), sample_n = 500L) {
  explainer <- build_dalex_explainer(model, x, y, predict_function, label)
  DALEX::model_profile(explainer, variables = variables, N = min(sample_n, nrow(x)), type = "partial")
}

compute_shap <- function(model, x, y, predict_function, label, simulations = 100L, observations = 20L, seed = 15L) {
  set.seed(seed)
  explainer <- build_dalex_explainer(model, x, y, predict_function, label)
  selected <- seq_len(min(observations, nrow(x)))
  pieces <- lapply(selected, function(i) {
    result <- DALEX::predict_parts(explainer, new_observation = x[i, , drop = FALSE], type = "shap", B = simulations)
    result$observation <- i
    as.data.frame(result)
  })
  values <- do.call(rbind, pieces)
  usable <- values[!values$variable_name %in% c("_baseline_", "_full_model_"), , drop = FALSE]
  importance <- data.table::as.data.table(usable)[, .(
    importance_shap_moyenne_absolue = mean(abs(contribution), na.rm = TRUE)), by = .(variable = variable_name)] |> as.data.frame()
  importance <- importance[order(-importance$importance_shap_moyenne_absolue), ]
  list(values = values, importance = importance)
}

explain_predictive_model <- function(model, x, y, predict_function, label,
                                     repetitions = 20L, shap_simulations = 100L) {
  list(global = compute_global_importance(model, x, y, predict_function, label, repetitions),
    partial_dependence = compute_partial_dependence(model, x, y, predict_function, label),
    shap = compute_shap(model, x, y, predict_function, label, shap_simulations))
}

econometric_ml_comparison_rules <- function() {
  data.frame(
    DIMENSION = c("Population", "Issue", "Temporalite", "Variable", "Echelle", "Incertitude", "Conclusion autorisee"),
    CONDITION = c("Meme population analytique", "Meme definition de Y", "Memes dates de mesure et horizon",
      "Meme information source, meme transformation", "Coefficient/odds ratio distinct d'une importance ou valeur SHAP",
      "IC econometrique et variabilite predictive presentes separement", "Convergences et divergences descriptives, jamais preuve causale"),
    stringsAsFactors = FALSE)
}

explainability_output_register <- function(available = FALSE) {
  data.frame(
    SORTIE = c("Importance globale", "Permutation importance", "Partial dependence", "SHAP", "Comparaison econometrique"),
    OUTIL = c("vip et SHAP moyen absolu", "vip et DALEX", "DALEX::model_profile", "DALEX::predict_parts(type='shap')", "Table de concordance conceptuelle"),
    PRODUITE = if (available) "OUI" else "NON",
    RAISON = if (available) "Modele et predictions hors echantillon disponibles" else
      "Aucun modele predictif entraine a l'etape 13; aucune explication empirique possible",
    stringsAsFactors = FALSE)
}

run_explainability_analysis <- function(modeling_results, econometric_results, root = here::here()) {
  available <- !is.null(modeling_results$trained_models)
  if (available) stop("Le modele final, ses donnees de reference et sa fonction de prediction doivent etre fournis explicitement.", call. = FALSE)
  output <- file.path(root, "outputs", "tables", "explainability"); fs::dir_create(output)
  write_utf8_csv(explainability_definitions(), file.path(output, "definitions.csv"))
  write_utf8_csv(explainability_data_contract(), file.path(output, "contrat_donnees.csv"))
  write_utf8_csv(econometric_ml_comparison_rules(), file.path(output, "regles_comparaison_econometrie.csv"))
  write_utf8_csv(explainability_output_register(available), file.path(output, "registre_sorties.csv"))
  dependency <- data.frame(OUTIL = c("vip", "DALEX", "fastshap"),
    STATUT = c("RETENU", "RETENU", "NON RETENU"),
    JUSTIFICATION = c("Importance par permutation compatible", "Permutation, PDP et SHAP compatibles",
      "Retire de CRAN en 2026 et echec d'installation source sous R 4.6; SHAP fourni par DALEX"))
  write_utf8_csv(dependency, file.path(output, "choix_outils.csv"))
  list(status = "NON EXPLICABLE - AUCUN MODELE PREDICTIF ENTRAINE", definitions = explainability_definitions(),
    contract = explainability_data_contract(), comparison_rules = econometric_ml_comparison_rules(),
    outputs = explainability_output_register(available), dependency = dependency, explanations = NULL)
}
