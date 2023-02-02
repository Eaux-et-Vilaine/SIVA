test_that("indicateur_avifaune_isac fonctionne", {
  expect_is(indicateur_avifaune_isac(dat = isac_dat, niveau_marais = 'guenrouet'),"factor")
})
