## code to prepare `dat2001` dataset goes here
# loaded from other project
datafile <- system.file("dat2001.Rdata", package = "SIVA")
load(datafile)
dat2001 <- dat
dat2001 <- dat2001 %>% dplyr::rename("horodate"="HoroDate")
dat2001 <- dat2001 %>% rename(tot_vol_vanne=tot_vol_barrage)
usethis::use_data(dat2001, overwrite = TRUE)
