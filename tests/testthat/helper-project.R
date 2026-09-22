project_root <- rprojroot::find_root(rprojroot::has_file(".here"))
r_files <- list.files(file.path(project_root, "R"), pattern = "[.]R$", full.names = TRUE)
invisible(lapply(r_files, source))
