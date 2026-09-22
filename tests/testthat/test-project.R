test_that("le manifeste de l'environnement respecte le contrat", {
  manifest <- build_environment_manifest()

  expect_s3_class(manifest, "data.frame")
  expect_equal(nrow(manifest), 1L)
  expect_identical(manifest$project, "DEPP_A5_Mobilite_Personnels")
  expect_true(manifest$renv_active)
  expect_match(manifest$r_version, "^[0-9]+\\.[0-9]+\\.[0-9]+$")
})
