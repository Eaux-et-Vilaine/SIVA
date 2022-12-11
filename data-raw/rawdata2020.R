## code to prepare `rawdata2020` dataset goes here
datafile <- system.file("rawdata2020.Rdata", package = "SIVA")
load(datafile)
rawdata2020 <- rawdata2020 %>% dplyr::rename("horodate"="HoroDate")
usethis::use_data(rawdata2020, overwrite = TRUE)
