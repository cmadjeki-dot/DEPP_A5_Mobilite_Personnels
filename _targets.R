library(targets)

# Commandes de supervision a lancer depuis la racine du projet :
# targets::tar_make()                    # execute uniquement les cibles perimees
# targets::tar_visnetwork()              # affiche le graphe et les statuts
# targets::tar_read(descriptive_results) # lit un resultat depuis le magasin
# targets::tar_read(pipeline_manifest)   # retourne le chemin du registre final

tar_option_set(
  packages = c(
    "broom", "data.table", "digest", "dplyr", "fs", "ggplot2", "gt",
    "sf", "survival", "yaml"
  ),
  format = "rds",
  error = "stop",
  memory = "transient",
  garbage_collection = TRUE
)

tar_source("R")

list(
  tar_target(
    sources,
    list(environment = build_environment_manifest(), manifest = read_source_manifest()),
    description = "Versions de l'environnement et manifeste des sources officielles"
  ),
  tar_target(import_results, run_all_imports(sources$manifest),
    description = "Import reproductible et provenance des fichiers raw"),
  tar_target(quality_report, run_quality_audit(sources$manifest, import_results),
    description = "Diagnostic qualite avant toute correction"),
  tar_target(cleaned_data, run_all_cleaning(sources$manifest, quality_report),
    description = "Donnees nettoyees sans modification des fichiers raw"),
  tar_target(feature_data, run_feature_engineering(cleaned_data),
    description = "Variables derivees documentees"),
  tar_target(panel_data, run_panel_strategy(feature_data),
    description = "Panel alternatif et audit de faisabilite personnel-annee"),

  tar_target(descriptive_results, run_descriptive_analysis(panel_data),
    description = "Effectifs, profils, evolutions et ratios descriptifs"),
  tar_target(trajectory_results, run_trajectory_analysis(panel_data),
    description = "Sequences de presence, sans assimilation a des trajectoires individuelles"),
  tar_target(territorial_results, run_territorial_analysis(descriptive_results),
    description = "Indicateurs territoriaux et effets de composition"),
  tar_target(econometric_results, run_econometric_analysis(panel_data),
    description = "Associations conditionnelles au niveau etablissement"),
  tar_target(survival_results, run_survival_analysis(panel_data),
    description = "Audit de faisabilite de l'analyse de survie"),
  tar_target(modeling_results, run_machine_learning_analysis(panel_data),
    description = "Audit ML et prevention des fuites temporelles"),
  tar_target(evaluation_results, run_model_evaluation(modeling_results),
    description = "Evaluation conditionnelle a des predictions hors echantillon"),
  tar_target(explainability_results,
    run_explainability_analysis(modeling_results, econometric_results),
    description = "Explicabilite conditionnelle a un modele predictif"),

  tar_target(
    figures,
    register_output_files(
      dependencies = list(descriptive_results, trajectory_results, territorial_results),
      directory = "outputs/figures",
      extensions = c("png", "svg", "pdf")
    ),
    format = "file",
    description = "Toutes les figures produites par les analyses"
  ),
  tar_target(
    tables,
    register_output_files(
      dependencies = list(
        quality_report, cleaned_data, feature_data, panel_data,
        descriptive_results, trajectory_results, territorial_results,
        econometric_results, survival_results, modeling_results,
        evaluation_results, explainability_results
      ),
      directory = "outputs/tables",
      extensions = c("csv", "rds")
    ),
    format = "file",
    description = "Toutes les tables et tous les modeles diffuses"
  ),
  tar_target(
    reports,
    render_pipeline_reports(
      dependencies = list(figures, tables),
      reports = c("reports/rapport_methodologique.qmd", "reports/note_information.qmd")
    ),
    format = "file",
    description = "Rapport methodologique et note d'information rendus avec Quarto"
  ),
  tar_target(
    dashboard_validation,
    run_dashboard_validation(
      descriptive_results, trajectory_results, territorial_results,
      econometric_results, survival_results, modeling_results,
      evaluation_results, explainability_results
    ),
    description = "Validation des artefacts consommes par le dashboard"
  ),
  tar_target(
    pipeline_manifest,
    write_pipeline_manifest(
      dependencies = list(reports, dashboard_validation),
      sources = sources,
      path = "outputs/pipeline_manifest.csv"
    ),
    format = "file",
    description = "Registre final des versions et artefacts du pipeline"
  )
)
