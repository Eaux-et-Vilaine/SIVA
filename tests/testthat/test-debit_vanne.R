

test_that("La fonction de calcul de debit-vanne fonctionne", {
load(system.file("param2012_2014.Rdata", package = "SIVA"))
load(system.file("dat2019.Rdata", package = "SIVA"))
# Calcul du débit d'une vanne
Qva1 <-
  debit_vanne(
    hvilaine = dat$niveauvilaineb[4000:5000],
    hmer = dat$niveaumerb[4000:5000],
    hvanne = dat$vanne1[4000:5000],
    canal = "ifsw",
    Cvg = param["Cvg"],
    Cvgs = param["Cvgs"],
    Cvw = param["Cvw"],
    loi_orificenoye = "ifws"
  )
expect_is(Qva1, "data.frame")
})
