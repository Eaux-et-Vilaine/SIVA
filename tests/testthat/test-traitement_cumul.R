test_that("traitement_cumul fonctionne", {
  cumbarrage <- rawdata2020[,grep("tot", colnames(rawdata2020))]
  expect_error(tt <- traitement_cumul(dat=cumbarrage, pattern="tot"), NA)
  # celui là doit renvoyer une error pour mauvais pattern
  expect_error(tt <- traitement_cumul(dat=cumbarrage))
})
