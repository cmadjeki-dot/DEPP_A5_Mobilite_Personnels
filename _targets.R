library(targets)

tar_option_set(
  packages = c("data.table", "digest", "fs", "yaml"),
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
  )
)
