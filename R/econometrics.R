# Econometrie associative sous contraintes des donnees ouvertes.

econometric_framework <- function() {
  data.frame(
    ELEMENT = c("Y", "X principales", "Population", "Periode", "Variables de controle", "Hypotheses", "Estimand", "Causalite"),
    DEFINITION = c(
      "Indicateur binaire: UAI observee en 2024 mais absente du fichier 2025",
      "Part d'ETP avec moins de 2 ans d'anciennete dans l'etablissement",
      "Etablissements du premier degre observes en 2024, avec covariables completes",
      "Covariables 2024 et presence dans le fichier 2025",
      "Log(1 + ETP), part de femmes, part agee de 50 ans ou plus et secteur",
      "H1: composition et taille sont associees a l'absence en 2025; H2: l'association d'anciennete peut varier selon le secteur",
      "Odds ratio conditionnel d'absence du fichier, au niveau UAI",
      "Association conditionnelle uniquement; aucune intervention ni assignation exogene"),
    stringsAsFactors = FALSE)
}

prepare_econometric_data <- function(panel) {
  dt <- data.table::as.data.table(panel)[annee == min(annee)]
  dt[, y_absence_2025 := as.integer(disparition_apres_observation)]
  dt[, `:=`(
    log_etp = log1p(etp_enseignants),
    anciennete_moins_2_10pp = 10 * part_anciennete_etab_moins_2,
    age_50_plus_10pp = 10 * part_age_50_plus,
    femmes_10pp = 10 * part_femmes,
    ep = factor(data.table::fifelse(appariement_annuaire & !is.na(education_prioritaire),
      data.table::fifelse(education_prioritaire, "REP_REP_PLUS", "HORS_EP"), "NON_APPARIE"),
      levels = c("HORS_EP", "REP_REP_PLUS", "NON_APPARIE")),
    secteur = factor(secteur), region = factor(code_region))]
  variables <- c("uai", "code_departement", "y_absence_2025", "log_etp", "femmes_10pp",
    "age_50_plus_10pp", "anciennete_moins_2_10pp", "secteur")
  result <- dt[, ..variables]
  result <- result[stats::complete.cases(result)]
  if (data.table::uniqueN(result$y_absence_2025) != 2L) stop("Y ne presente pas deux modalites.", call. = FALSE)
  as.data.frame(result)
}

fit_econometric_models <- function(data) {
  formula_main <- y_absence_2025 ~ log_etp + anciennete_moins_2_10pp +
    age_50_plus_10pp + femmes_10pp + secteur
  formula_interaction <- update(formula_main, . ~ . + anciennete_moins_2_10pp:secteur)
  list(
    principal = stats::glm(formula_main, data = data, family = stats::binomial(link = "logit")),
    interaction = stats::glm(formula_interaction, data = data, family = stats::binomial(link = "logit")),
    probit = stats::glm(formula_main, data = data, family = stats::binomial(link = "probit"))
  )
}

tidy_econometric_coefficients <- function(model) {
  coefficients <- broom::tidy(model)
  coefficients$conf.low <- coefficients$estimate - stats::qnorm(.975) * coefficients$std.error
  coefficients$conf.high <- coefficients$estimate + stats::qnorm(.975) * coefficients$std.error
  coefficients$odds_ratio <- exp(coefficients$estimate)
  coefficients$or_ic95_basse <- exp(coefficients$conf.low)
  coefficients$or_ic95_haute <- exp(coefficients$conf.high)
  coefficients$interpretation <- ifelse(coefficients$term == "(Intercept)",
    "Niveau de reference; interpretation substantielle limitee",
    "Association conditionnelle toutes variables incluses maintenues constantes; non causale")
  coefficients
}

numeric_vif <- function(data, variables) {
  x <- data[, variables, drop = FALSE]
  do.call(rbind, lapply(variables, function(variable) {
    others <- setdiff(variables, variable)
    r2 <- summary(stats::lm(stats::reformulate(others, response = variable), data = x))$r.squared
    data.frame(variable = variable, vif = 1 / (1 - r2), seuil_alerte = 5,
      statut = ifelse(is.finite(r2) && 1 / (1 - r2) < 5, "OK", "ALERTE"))
  }))
}

auc_rank <- function(y, probability) {
  positives <- sum(y == 1); negatives <- sum(y == 0)
  if (!positives || !negatives) return(NA_real_)
  (sum(rank(probability, ties.method = "average")[y == 1]) - positives * (positives + 1) / 2) /
    (positives * negatives)
}

calibration_table <- function(model, groups = 10L) {
  p <- stats::fitted(model); y <- stats::model.response(stats::model.frame(model))
  breaks <- unique(stats::quantile(p, probs = seq(0, 1, length.out = groups + 1), na.rm = TRUE))
  group <- cut(p, breaks = breaks, include.lowest = TRUE, ordered_result = TRUE)
  data.table::data.table(groupe = group, observe = y, predit = p)[,
    .(observations = .N, evenements = sum(observe), taux_observe = mean(observe), probabilite_predite = mean(predit)),
    by = groupe] |> as.data.frame()
}

econometric_diagnostics <- function(models, data) {
  main <- models$principal
  p <- stats::fitted(main); y <- data$y_absence_2025
  linear <- stats::predict(main, type = "link")
  link_test <- stats::glm(y ~ linear + I(linear^2), family = stats::binomial())
  interaction_test <- stats::anova(main, models$interaction, test = "Chisq")
  mm <- stats::model.matrix(main)[, -1, drop = FALSE]
  scaled <- scale(mm); scaled <- scaled[, apply(scaled, 2, function(z) all(is.finite(z))), drop = FALSE]
  data.frame(
    DIAGNOSTIC = c("Evenements Y=1", "Taux d'evenement", "AUC apparente", "Score de Brier",
      "Deviance / ddl residuels", "Condition number", "Link test: terme quadratique p", "Interaction anciennete x secteur: test LR p",
      "Observations influentes (Cook > 4/n)", "Coefficients finis", "Convergence des trois modeles"),
    RESULTAT = c(sum(y), mean(y), auc_rank(y, p), mean((y - p)^2), stats::deviance(main) / stats::df.residual(main),
      kappa(scaled, exact = TRUE), stats::coef(summary(link_test))["I(linear^2)", "Pr(>|z|)"],
      interaction_test$`Pr(>Chi)`[2], sum(stats::cooks.distance(main) > 4 / nrow(data)), all(is.finite(stats::coef(main))),
      sum(vapply(models, function(model) isTRUE(model$converged), logical(1)))),
    INTERPRETATION = c("Information effective pour l'issue rare", "Part des UAI 2024 absentes en 2025",
      "Discrimination interne, sans validation externe", "Erreur quadratique probabiliste", "Dispersion descriptive",
      "Multicolinearite globale de la matrice", "Une faible p-valeur signale une specification possiblement incomplete",
      "Teste l'apport de l'interaction secteur x anciennete", "Points a examiner; aucun retrait silencieux",
      "Controle de separation numerique", "Trois convergences attendues"),
    stringsAsFactors = FALSE)
}

robustness_table <- function(models) {
  extract <- function(model, name, exponentiate = TRUE) {
    x <- broom::tidy(model)
    x$conf.low <- x$estimate - stats::qnorm(.975) * x$std.error
    x$conf.high <- x$estimate + stats::qnorm(.975) * x$std.error
    if (exponentiate) x[c("estimate", "conf.low", "conf.high")] <- exp(x[c("estimate", "conf.low", "conf.high")])
    x <- x[x$term == "anciennete_moins_2_10pp", c("term", "estimate", "conf.low", "conf.high", "p.value")]
    x$modele <- name; x$echelle <- if (exponentiate) "odds ratio" else "coefficient probit"; x
  }
  rbind(extract(models$principal, "Logit principal"), extract(models$interaction, "Logit avec interaction"),
    extract(models$probit, "Probit", exponentiate = FALSE))
}

econometric_limits <- function() {
  data.frame(
    LIMITE = c("Issue observee", "Granularite", "Temporalite", "Education prioritaire", "Endogeneite", "Validation"),
    CONSEQUENCE = c("Absence du fichier != fermeture, sortie ou mobilite", "UAI et non personnel individuel",
      "Deux millesimes seulement", "Statut issu d'un snapshot 2026, posterieur a l'issue", "Confusion residuelle et selection possibles",
      "Performances apparentes sur l'echantillon d'estimation"),
    DECISION = c("Nommer l'issue exactement", "Interdire l'interpretation individuelle", "Ne pas utiliser plm/fixest",
      "Exclure cette variable du modele pour eviter une fuite temporelle", "Ne revendiquer aucun effet causal", "Ne pas promettre de prediction hors echantillon"),
    stringsAsFactors = FALSE)
}

run_econometric_analysis <- function(panel_data = NULL, root = here::here()) {
  panel <- readRDS(file.path(root, "data", "processed", "panel", "etablissement_annee_panel.rds"))
  data <- prepare_econometric_data(panel)
  models <- fit_econometric_models(data)
  coefficients <- tidy_econometric_coefficients(models$principal)
  vif <- numeric_vif(data, c("log_etp", "anciennete_moins_2_10pp", "age_50_plus_10pp", "femmes_10pp"))
  diagnostics <- econometric_diagnostics(models, data)
  calibration <- calibration_table(models$principal)
  robustness <- robustness_table(models)
  output <- file.path(root, "outputs", "tables", "econometrics"); fs::dir_create(output)
  write_utf8_csv(econometric_framework(), file.path(output, "cadre_econometrique.csv"))
  write_utf8_csv(coefficients, file.path(output, "coefficients_logit.csv"))
  write_utf8_csv(vif, file.path(output, "multicolinearite_vif.csv"))
  write_utf8_csv(diagnostics, file.path(output, "diagnostics.csv"))
  write_utf8_csv(calibration, file.path(output, "calibration.csv"))
  write_utf8_csv(robustness, file.path(output, "robustesse.csv"))
  write_utf8_csv(econometric_limits(), file.path(output, "limites.csv"))
  saveRDS(models, file.path(output, "modeles.rds"))
  list(framework = econometric_framework(), n = nrow(data), coefficients = coefficients, vif = vif,
    diagnostics = diagnostics, calibration = calibration, robustness = robustness, limits = econometric_limits())
}
