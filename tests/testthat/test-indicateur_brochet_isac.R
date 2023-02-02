test_that("indicateur_brochet_isac fonctionne", {
  expect_is(indicateur_brochet_isac(dat = isac_dat, niveau_marais = 'guenrouet'),"factor")
  # les dates ne doivent pas être modifiées d'un mois
  expect_error(indicateur_brochet_isac(dat = isac_dat, debut_ponte = "01/01", niveau_marais = 'guenrouet'))
})
