
test_that("traitement_siva fonctionne", {
 load(system.file("rawdata2020.Rdata", package="SIVA"))
 cordata2020 <- traitement_siva(dat=rawdata2020)
 # si les valeurs sont transformées les valeurs sont plus loin de 1e9
 expect_gt(max(cordata2020$tot_vol_vanne, na.rm=T),6e5)
})
