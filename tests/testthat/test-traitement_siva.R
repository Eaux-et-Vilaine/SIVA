
test_that("traitement_siva fonctionne", {
 load(system.file("rawdata2020.Rdata", package="SIVA"))
 cordata2020 <- traitement_siva(dat=rawdata2020)
 expect_gt(max(cordata2020$tot_vol_vanne, na.rm=T),6e5)
})
