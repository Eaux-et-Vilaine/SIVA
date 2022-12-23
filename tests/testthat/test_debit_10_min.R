test_that("debit_10_min fonctionne", {
  Q10 <- debit_10_min(debit_barrage=dat2019)
  expect_is(Q10,"data.frame")

})