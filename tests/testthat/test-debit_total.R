
test_that("debit_total fonctionne", {
  debit_barrage <- traitement_siva(dat2019)
  Q12345 <- debit_total(param, param0 = param, debit_barrage)
  expect_is(Q12345,"data.frame")
})
