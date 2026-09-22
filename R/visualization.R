# Validation des artefacts consommes par le dashboard.

dashboard_required_artifacts <- function(root = here::here()) {
  relative <- c(
    "outputs/tables/descriptive/effectifs.csv", "outputs/tables/descriptive/age.csv",
    "outputs/tables/descriptive/anciennete.csv", "outputs/tables/descriptive/mobilite.csv",
    "outputs/tables/territorial/indicateurs_departements_2025.csv",
    "outputs/tables/territorial/indicateurs_academies_2025.csv",
    "outputs/tables/territorial/indicateurs_regions_2025.csv",
    "outputs/tables/trajectories/profils_sequences_presence.csv",
    "outputs/tables/econometrics/coefficients_logit.csv",
    "outputs/tables/modeling/audit_faisabilite.csv",
    "outputs/tables/survival/audit_faisabilite.csv",
    "outputs/tables/evaluation/statut_evaluation.csv",
    "outputs/tables/explainability/registre_sorties.csv",
    "outputs/figures/territorial/age_50_plus.png",
    "outputs/figures/territorial/anciennete_moins_2.png",
    "outputs/figures/territorial/education_prioritaire.png")
  data.frame(artefact = relative, chemin = file.path(root, relative), stringsAsFactors = FALSE)
}

validate_dashboard_artifacts <- function(root = here::here()) {
  manifest <- dashboard_required_artifacts(root)
  manifest$existe <- file.exists(manifest$chemin)
  manifest$taille_octets <- ifelse(manifest$existe, file.info(manifest$chemin)$size, NA_real_)
  manifest$statut <- ifelse(manifest$existe & manifest$taille_octets > 0, "OK", "ERREUR")
  manifest$chemin <- NULL
  manifest
}

run_dashboard_validation <- function(..., root = here::here()) {
  validation <- validate_dashboard_artifacts(root)
  if (any(validation$statut == "ERREUR")) stop("Artefacts du dashboard manquants.", call. = FALSE)
  if (!file.exists(file.path(root, "dashboard", "app.R"))) stop("dashboard/app.R absent.", call. = FALSE)
  validation
}
