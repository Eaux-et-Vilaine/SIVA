## code to prepare `param_2012_2014` dataset goes here

usethis::use_data_raw(name="param2012_2014", open = TRUE)
datafile <- system.file("param2012_2014.Rdata", package = "SIVA")
load(datafile)
usethis::use_data(param, overwrite = TRUE)
