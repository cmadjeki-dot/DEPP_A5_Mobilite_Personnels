library(targets)

tar_option_set(
  packages = character(),
  format = "rds",
  error = "stop"
)

tar_source("R")

list(
  tar_target(
    environment_manifest,
    build_environment_manifest()
  )
)
