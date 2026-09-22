# Fonctions transversales du projet.

#' Construire un manifeste minimal de l'environnement d'execution
#'
#' @return Un data.frame d'une ligne, sans chemin absolu.
build_environment_manifest <- function() {
  data.frame(
    project = "DEPP_A5_Mobilite_Personnels",
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    renv_active = nzchar(Sys.getenv("RENV_PROJECT")),
    stringsAsFactors = FALSE
  )
}
