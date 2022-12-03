## code to prepare `dat2019` dataset goes here
datafile <- system.file("dat2019.Rdata", package = "SIVA")
load(datafile)
dat2019 <- dat
usethis::use_data(dat2019, overwrite = TRUE)
