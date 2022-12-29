
test_that("debit_total fonctionne", {
  Qj <- debit_journalier(debit_barrage=dat2019, type = "recalcule")
  expect_is(Qj,"data.frame")
  Q2j <- debit_journalier(debit_barrage=dat2019, type = "barrage_volume")
  expect_is(Qj,"data.frame")
  #plot(Q2j$vol_passe)
  Q3j <- debit_journalier(debit_barrage=dat2019, type = "barrage_debit")
  expect_is(Qj,"data.frame")
  expect_equal(nrow(Qj),nrow(Q2j))
  expect_equal(nrow(Q2j),nrow(Q3j))
  expect_error(Qj <- 
                 debit_journalier(debit_barrage=dat2019, type = "barbapapa"))
 })
