## code to prepare `dat2019` dataset goes here
datafile <- system.file("dat2019.Rdata", package = "SIVA")
load(datafile)

dat2019 <- dat2019 %>% dplyr::rename("horodate"="HoroDate") 
dat2019 <- dat2019 %>%   select(-c("day","dm","week","diff","lev3","lev25",
                                   "lev2","lev1.5","diffniveaumer","daypb25","daypb3","p","r","resvilinfm03","resvilinfm2","daypbvilinf2"))  
usethis::use_data(dat2019, overwrite = TRUE)
