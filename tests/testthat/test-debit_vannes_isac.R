test_that("debit_vannes_isac works", {
   debit_isac <-
     debit_vannes_isac(
       hamont = isac_dat$isac_amont2,
       haval = isac_dat$pontdecran,
       hvanne1 = isac_dat$isac_position_vanne_1,
       hvanne2 = isac_dat$isac_position_vanne_2
     )
   expect_equal(nrow(debit_isac),52588)
})
