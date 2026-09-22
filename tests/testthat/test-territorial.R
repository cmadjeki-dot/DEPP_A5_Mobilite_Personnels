test_that("l'agregation territoriale utilise les denominateurs observes", {
  x <- data.frame(annee = 2025L, code_departement = c("01", "01"), nom_departement = "A",
    uai = c("a", "b"), etp_d_enseignants_hommes_et_femmes = c(10, 30), etp_de_femmes_enseignantes = c(6, 18),
    etp_d_enseignants_de_50_ans_ou_plus = c(4, NA),
    etp_d_enseignants_ayant_une_anciennete_dans_l_etablissement_de_moins_de_2_ans = c(2, 3),
    etp_d_enseignants_ayant_une_anciennete_dans_l_etablissement_de_8_ans_ou_plus = c(5, 20))
  out <- aggregate_personnel_territory(x, "departement", 2025)
  expect_equal(out$part_age_50_plus, .4); expect_equal(out$part_age_50_plus_denominateur, 10)
  expect_equal(out$part_age_50_plus_couverture, .25); expect_equal(out$part_anciennete_moins_2, 5 / 40)
})

test_that("les metadonnees de carte sont completes", {
  labels <- territorial_map_labels("Titre", "Indicateur", "Denominateur", "Champ", 2025, "Source", "Note")
  expect_match(labels$subtitle, "Indicateur"); expect_match(labels$subtitle, "Denominateur")
  expect_match(labels$subtitle, "Champ"); expect_match(labels$subtitle, "2025"); expect_match(labels$caption, "Source")
})

test_that("l'effet de composition distingue moyenne ponderee et moyenne simple", {
  x <- data.frame(part_age_50_plus = c(.1, .5), part_age_50_plus_numerateur = c(1, 50), part_age_50_plus_denominateur = c(10, 100),
    part_anciennete_moins_2 = c(.2, .4), part_anciennete_moins_2_numerateur = c(2, 40), part_anciennete_moins_2_denominateur = c(10, 100),
    part_anciennete_8_plus = c(.6, .3), part_anciennete_8_plus_numerateur = c(6, 30), part_anciennete_8_plus_denominateur = c(10, 100))
  out <- build_composition_effects(x)
  expect_equal(out$moyenne_nationale_ponderee_etp[[1]], 51 / 110); expect_equal(out$moyenne_simple_departements[[1]], .3)
})
