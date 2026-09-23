# Déploiement du dashboard sur shinyapps.io.
# Les identifiants restent dans l'environnement local et ne sont jamais versionnés.

required_credentials <- c(
  account = "SHINYAPPS_ACCOUNT",
  token = "SHINYAPPS_TOKEN",
  secret = "SHINYAPPS_SECRET"
)

credentials <- Sys.getenv(required_credentials)
missing_credentials <- names(credentials)[credentials == ""]

if (length(missing_credentials) > 0L) {
  stop(
    "Identifiants shinyapps.io manquants : ",
    paste(required_credentials[missing_credentials], collapse = ", "),
    ". Configurez-les dans votre fichier .Renviron local.",
    call. = FALSE
  )
}

app_files <- c(
  ".here",
  "DESCRIPTION",
  "renv.lock",
  "dashboard/app.R",
  "outputs/tables/descriptive/effectifs.csv",
  "outputs/tables/descriptive/age.csv",
  "outputs/tables/descriptive/anciennete.csv",
  "outputs/tables/descriptive/mobilite.csv",
  "outputs/tables/territorial/indicateurs_departements_2025.csv",
  "outputs/tables/territorial/indicateurs_academies_2025.csv",
  "outputs/tables/territorial/indicateurs_regions_2025.csv",
  "outputs/tables/trajectories/profils_sequences_presence.csv",
  "outputs/tables/econometrics/coefficients_logit.csv",
  "outputs/tables/modeling/audit_faisabilite.csv",
  "outputs/tables/survival/audit_faisabilite.csv",
  "outputs/tables/evaluation/statut_evaluation.csv",
  "outputs/tables/explainability/registre_sorties.csv",
  "outputs/tables/diagnostic_qualite.csv",
  list.files(
    "outputs/figures/territorial",
    pattern = "[.]png$",
    full.names = TRUE
  )
)

missing_files <- app_files[!file.exists(app_files)]
if (length(missing_files) > 0L) {
  stop(
    "Artefacts préparés manquants : ",
    paste(missing_files, collapse = ", "),
    ". Exécutez targets::tar_make() avant le déploiement.",
    call. = FALSE
  )
}

rsconnect::setAccountInfo(
  name = unname(credentials[["account"]]),
  token = unname(credentials[["token"]]),
  secret = unname(credentials[["secret"]])
)

rsconnect::deployApp(
  appDir = ".",
  appPrimaryDoc = "dashboard/app.R",
  appFiles = app_files,
  appName = "depp-a5-mobilite-personnels",
  appTitle = "Observatoire des parcours et mobilités des personnels",
  server = "shinyapps.io",
  forceUpdate = TRUE
)
