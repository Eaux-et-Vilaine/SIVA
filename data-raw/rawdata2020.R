## code to prepare `rawdata2020` dataset goes here
datafile <- system.file("rawdata2020.Rdata", package = "SIVA")
load(datafile)
usethis::use_data(rawdata2020, overwrite = TRUE)
