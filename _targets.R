library(targets)

tar_option_set(
  packages = c("broom", "data.table", "digest", "dplyr", "fs", "ggplot2", "gt", "sf", "survival", "yaml"),
  format = "rds",
  error = "stop"
)

tar_source("R")

list(
  tar_target(
    environment_manifest,
    build_environment_manifest()
  ),
  tar_target(source_manifest, read_source_manifest()),
  tar_target(
    import_results,
    run_all_imports(source_manifest)
  ),
  tar_target(
    quality_report,
    run_quality_audit(source_manifest, import_results)
  ),
  tar_target(
    cleaned_data,
    run_all_cleaning(source_manifest, quality_report)
  ),
  tar_target(
    feature_data,
    run_feature_engineering(cleaned_data)
  ),
  tar_target(
    panel_data,
    run_panel_strategy(feature_data)
  ),
  tar_target(
    descriptive_results,
    run_descriptive_analysis(panel_data)
  ),
  tar_target(
    trajectory_results,
    run_trajectory_analysis(panel_data)
  ),
  tar_target(
    territorial_results,
    run_territorial_analysis(descriptive_results)
  ),
  tar_target(
    econometric_results,
    run_econometric_analysis(panel_data)
  ),
  tar_target(
    survival_results,
    run_survival_analysis(panel_data)
  )
)
