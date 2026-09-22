# Machine learning temporel: audit, prevention des fuites et pipeline conditionnel.

ml_scientific_definition <- function() {
  data.frame(
    ELEMENT = c("Cible", "Unite", "Instant des predicteurs", "Horizon", "Train", "Validation", "Test", "Metrique principale"),
    DEFINITION = c("Mobilite individuelle entre t et t+1, codee oui/non", "Personnel x annee t",
      "Variables connues au plus tard a t", "Une annee", "Annees d'origine les plus anciennes",
      "Annees d'origine intermediaires, jamais anterieures au train", "Derniere annee d'origine, utilisee une seule fois",
      "Aire sous la courbe precision-rappel, adaptee a une cible potentiellement rare"),
    stringsAsFactors = FALSE)
}

audit_ml_feasibility <- function(panel) {
  years <- sort(unique(panel$annee))
  required <- c("id_personnel", "mobilite_t_plus_1")
  present <- required %in% names(panel)
  data.frame(
    CONTROLE = c("Identifiant personnel longitudinal", "Cible mobilite a t+1", "Annees observees",
      "Transitions pour train/validation/test", "Predicteurs individuels mesures a t", "Sortie distinguee d'une disparition"),
    REQUIS = c("id_personnel", "mobilite_t_plus_1", ">= 4 annees", ">= 3 annees d'origine",
      "Variables individuelles datees", "Statut individuel fiable"),
    RESULTAT = c(ifelse(present[1], "Disponible", "Absent"), ifelse(present[2], "Disponible", "Absent"),
      paste(length(years), "annees"), paste(max(length(years) - 1L, 0L), "transition"), "Absent", "Non"),
    STATUT = c(ifelse(present, "OK", "BLOQUANT"), ifelse(length(years) >= 4, "OK", "BLOQUANT"),
      ifelse(length(years) - 1L >= 3, "OK", "BLOQUANT"), "BLOQUANT", "BLOQUANT"),
    ACTION = c("Obtenir un identifiant pseudonymise stable", "Construire la cible depuis les positions t et t+1",
      "Etendre la profondeur historique", "Reserver la derniere transition au test",
      "Obtenir les covariables internes a la date t", "Obtenir les statuts administratifs fiables"),
    stringsAsFactors = FALSE)
}

ml_leakage_rules <- function() {
  data.frame(
    RISQUE = c("Information future", "Pretraitement global", "Validation aleatoire", "Doublon personnel", "Snapshot posterieur", "Test reutilise"),
    REGLE = c("Exclure toute variable calculee apres t ou contenant l'issue t+1",
      "Estimer imputation, niveaux et normalisation sur chaque echantillon d'analyse uniquement",
      "Creer les folds par annees croissantes", "Garder toutes les lignes d'une personne du meme cote d'une coupure si chevauchement",
      "Exclure le statut EP 2026 pour predire une mobilite anterieure", "N'evaluer le test final qu'apres choix du workflow"),
    IMPLEMENTATION = c("audit_predictor_leakage()", "recipe preparee dans le workflow resample", "temporal_resamples()",
      "controle des identifiants", "liste d'exclusion explicite", "last_fit() apres tuning"),
    stringsAsFactors = FALSE)
}

audit_predictor_leakage <- function(data, outcome = "mobilite_t_plus_1",
                                    allowed_id = c("id_personnel", "annee")) {
  candidate <- setdiff(names(data), c(outcome, allowed_id))
  forbidden_pattern <- "(^|_)(t_plus_1|future|suivant|suivante|apres|date_fin|evenement)($|_)"
  forbidden_exact <- c("education_prioritaire_2026", "appariement_annuaire_2026")
  unique(c(candidate[grepl(forbidden_pattern, candidate, ignore.case = TRUE)], intersect(candidate, forbidden_exact)))
}

validate_ml_input <- function(data) {
  required <- c("id_personnel", "annee", "mobilite_t_plus_1")
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("Variables ML manquantes: ", paste(missing, collapse = ", "), call. = FALSE)
  if (length(unique(data$annee)) < 3L) stop("Au moins trois annees d'origine sont requises pour train/validation/test.", call. = FALSE)
  if (any(!data$mobilite_t_plus_1 %in% c(0, 1, "non", "oui"))) stop("La cible doit etre binaire.", call. = FALSE)
  leakage <- audit_predictor_leakage(data)
  if (length(leakage)) stop("Fuite potentielle detectee: ", paste(leakage, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

temporal_partition <- function(data) {
  validate_ml_input(data)
  data$mobilite_t_plus_1 <- factor(ifelse(as.character(data$mobilite_t_plus_1) %in% c("1", "oui"), "oui", "non"),
    levels = c("non", "oui"))
  years <- sort(unique(data$annee)); test_year <- tail(years, 1); validation_year <- tail(years, 2)[1]
  list(train = data[data$annee < validation_year, , drop = FALSE],
    validation = data[data$annee == validation_year, , drop = FALSE],
    test = data[data$annee == test_year, , drop = FALSE],
    years = data.frame(ensemble = c("train", "validation", "test"),
      annees = c(paste(years[years < validation_year], collapse = ", "), validation_year, test_year)))
}

temporal_resamples <- function(data) {
  years <- sort(unique(data$annee))
  if (length(years) < 2L) stop("Deux annees d'origine au minimum sont requises pour le resampling.", call. = FALSE)
  splits <- lapply(years[-1], function(year) {
    rsample::make_splits(list(analysis = which(data$annee < year), assessment = which(data$annee == year)), data)
  })
  rsample::manual_rset(splits, ids = paste0("validation_", years[-1]))
}

final_temporal_split <- function(data) {
  partition <- temporal_partition(data)
  combined <- rbind(partition$train, partition$validation, partition$test)
  test_year <- max(combined$annee)
  rsample::make_splits(list(analysis = which(combined$annee < test_year),
    assessment = which(combined$annee == test_year)), combined)
}

build_ml_recipe <- function(training_data) {
  recipes::recipe(mobilite_t_plus_1 ~ ., data = training_data) |>
    recipes::update_role(id_personnel, annee, new_role = "identifiant") |>
    recipes::step_rm(recipes::has_role("identifiant")) |>
    recipes::step_unknown(recipes::all_nominal_predictors()) |>
    recipes::step_novel(recipes::all_nominal_predictors()) |>
    recipes::step_impute_median(recipes::all_numeric_predictors()) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())
}

build_ml_specifications <- function() {
  list(
    regression_logistique = parsnip::logistic_reg(penalty = tune::tune(), mixture = tune::tune()) |>
      parsnip::set_engine("glmnet") |> parsnip::set_mode("classification"),
    random_forest = parsnip::rand_forest(mtry = tune::tune(), min_n = tune::tune(), trees = 1000) |>
      parsnip::set_engine("ranger", importance = "permutation", probability = TRUE) |> parsnip::set_mode("classification"),
    gradient_boosting = parsnip::boost_tree(mtry = tune::tune(), tree_depth = tune::tune(),
      learn_rate = tune::tune(), min_n = tune::tune(), loss_reduction = tune::tune(), trees = 1000) |>
      parsnip::set_engine("xgboost") |> parsnip::set_mode("classification"))
}

build_ml_workflows <- function(training_data) {
  recipe <- build_ml_recipe(training_data); specifications <- build_ml_specifications()
  lapply(specifications, function(specification) workflows::workflow() |>
    workflows::add_recipe(recipe) |> workflows::add_model(specification))
}

tune_ml_workflows <- function(data, grid_size = 20L, seed = 13L) {
  partition <- temporal_partition(data)
  development <- rbind(partition$train, partition$validation)
  folds <- temporal_resamples(development)
  workflows <- build_ml_workflows(partition$train)
  metrics <- yardstick::metric_set(yardstick::pr_auc, yardstick::roc_auc, yardstick::bal_accuracy)
  set.seed(seed)
  tuned <- lapply(workflows, function(workflow) tune::tune_grid(workflow, resamples = folds,
    grid = grid_size, metrics = metrics, control = tune::control_grid(save_pred = TRUE)))
  list(partition = partition, folds = folds, workflows = workflows, tuned = tuned,
    selection_metric = "pr_auc", test_policy = "Test reserve; last_fit seulement apres selection finale")
}

finalize_and_test_ml <- function(tuning_result, workflow, data, metric = "pr_auc") {
  best <- tune::select_best(tuning_result, metric = metric)
  final_workflow <- tune::finalize_workflow(workflow, best)
  final_split <- final_temporal_split(data)
  result <- tune::last_fit(final_workflow, split = final_split,
    metrics = yardstick::metric_set(yardstick::pr_auc, yardstick::roc_auc, yardstick::bal_accuracy))
  list(best_parameters = best, workflow = final_workflow, last_fit = result,
    test_metrics = tune::collect_metrics(result), test_predictions = tune::collect_predictions(result))
}

ml_choice_register <- function(feasible = FALSE) {
  data.frame(
    COMPOSANT = c("Train/test", "Recipe", "Resampling", "Pretraitement", "Regression logistique", "Random Forest",
      "Gradient Boosting", "Tuning", "Evaluation finale"),
    CHOIX = c("Coupures par annees d'origine", "Roles ID/temps exclus des predicteurs", "Fenetres temporelles croissantes",
      "Niveaux inconnus/nouveaux, imputation mediane, zero variance, normalisation", "glmnet penalisee",
      "ranger, 1000 arbres", "xgboost, 1000 iterations maximales", "Grille sur folds temporels; selection PR-AUC",
      "Derniere annee d'origine, une seule fois"),
    JUSTIFICATION = c("Le futur ne doit jamais entrainer le passe", "Eviter ID et temps comme raccourcis",
      "Reproduire une utilisation prospective", "Parametres appris dans chaque fold uniquement", "Reference interpretable et regularisee",
      "Non-linearites et interactions", "Non-linearites sequentielles", "Cible rare probable", "Estimation non reutilisee"),
    STATUT = if (feasible) "PRET" else "NON EXECUTE - DONNEES INSUFFISANTES", stringsAsFactors = FALSE)
}

run_machine_learning_analysis <- function(panel_data = NULL, root = here::here()) {
  panel <- readRDS(file.path(root, "data", "processed", "panel", "etablissement_annee_panel.rds"))
  feasibility <- audit_ml_feasibility(panel)
  feasible <- !any(feasibility$STATUT == "BLOQUANT")
  if (feasible) stop("Une table personnel x annee valide doit etre fournie explicitement avant entrainement.", call. = FALSE)
  output <- file.path(root, "outputs", "tables", "modeling"); fs::dir_create(output)
  write_utf8_csv(ml_scientific_definition(), file.path(output, "definition_cible.csv"))
  write_utf8_csv(feasibility, file.path(output, "audit_faisabilite.csv"))
  write_utf8_csv(ml_leakage_rules(), file.path(output, "prevention_fuites.csv"))
  write_utf8_csv(ml_choice_register(feasible), file.path(output, "choix_modelisation.csv"))
  list(status = "MOBILITE T+1 NON PREDICTIBLE AVEC L'OPEN DATA", definition = ml_scientific_definition(),
    feasibility = feasibility, leakage = ml_leakage_rules(), choices = ml_choice_register(feasible),
    trained_models = NULL, metrics = NULL)
}
