## code to prepare `variable` dataset goes here

if (!exists("mainpass"))
  mainpass <- getPass::getPass(msg = "main password")
if (!exists("hostmysql")) {
  hostmysql. <- getPass::getPass(msg = "Saisir host")
  # ci dessous pour ne pas redemander au prochain tour
  hostmysql <- encrypt_string(string = hostmysql., key = mainpass)
} else {
  hostmysql. <- decrypt_string(string = hostmysql, key = mainpass)
}
if (!exists("pwdmysql")) {
  pwdmysql. <- getPass::getPass(msg = "Saisir password")
  pwdmysql <- encrypt_string(string = pwdmysql., key = mainpass)
}  else {
  # pass should be loaded
  pwdmysql. <- decrypt_string(string = pwdmysql, key = mainpass)
}
if (!exists("umysql")) {
  umysql. <- getPass::getPass(msg = "Saisir user")
  umysql <- encrypt_string(string = umysql., key = mainpass)
} else {
  umysql. <- decrypt_string(string = umysql, key = mainpass)
}
# attention il faut avaoir définit mainpass <- "xxxxx"
options(
    stacomiR.dbname = "archive_IAV",
    # TODO not used....
    stacomiR.host = hostmysql.,
    stacomiR.password = pwdmysql.,
    stacomiR.user = umysql.,
    stacomiR.ODBClink = "archive_IAV"
)



skip_if_not(interactive())
if (!exists("mainpass"))
  mainpass <- getPass::getPass(msg = "main password")
if (!exists("hostmysql")) {
  hostmysql. <- getPass::getPass(msg = "Saisir host")
  # ci dessous pour ne pas redemander au prochain tour
  hostmysql <- encrypt_string(string = hostmysql., key = mainpass)
} else {
  hostmysql. <- decrypt_string(string = hostmysql, key = mainpass)
}
if (!exists("pwdmysql")) {
  pwdmysql. <- getPass::getPass(msg = "Saisir password")
  pwdmysql <- encrypt_string(string = pwdmysql., key = mainpass)
}  else {
  # pass should be loaded
  pwdmysql. <- decrypt_string(string = pwdmysql, key = mainpass)
}
if (!exists("umysql")) {
  umysql. <- getPass::getPass(msg = "Saisir user")
  umysql <- encrypt_string(string = umysql., key = mainpass)
} else {
  umysql. <- decrypt_string(string = umysql, key = mainpass)
}

pool <- pool::dbPool(
    drv = RMariaDB::MariaDB(),
    dbname = "archive_IAV",
    host = hostmysql.,
    username = umysql.,
    password = pwdmysql.,
    port = 3306
)


requete<-
    stringr::str_c(
        "SELECT  Tag, TagStation, Libelle, MinStation, MaxStation, 
            MinPC, MaxPC, Format, Unite, TableHisto, TableBilan, Categorie, 
            tag_type_domaine, tag_domaine, tag_sous_domaine, tag_bassin, 
            tag_sous_bassin, tag_sous_bassin_loc
            FROM SIVA_R.svg2015_variable"
    )
variable <- DBI::dbGetQuery(pool,requete)



Encoding(variable$Libelle) <- "latin1"
variable$Libelle <- iconv(
    variable$Libelle, 
    "latin1", 
    "UTF-8"
)
Encoding(variable$Unite) <- "latin1"
variable$Unite <- iconv(
    variable$Unite, 
    "latin1", 
    "UTF-8"
)
colnames(variable) <- tolower(colnames(variable))
usethis::use_data(variable, overwrite = TRUE)



# niveaux ---------------------

library(readxl)
niveau <- read_excel("data-raw/variables.xlsx", sheet="niveau")
#Encoding(niveau$Libelle) <- "latin1"
#niveau$Libelle <- iconv(
#    niveau$Libelle, 
#    "latin1", 
#    "UTF-8"
#)
colnames(niveau) <- tolower(colnames(niveau))
usethis::use_data(niveau, overwrite = TRUE)


# debit --------------------------------------
library(readxl)
debit <- read_excel("data-raw/variables.xlsx",sheet= "debit" )
colnames(debit) <- tolower(colnames(debit))
usethis::use_data(debit, overwrite = TRUE)

# Isac -----------------------------------------------------------



# variable %>%dplyr::filter(tag %in% 
#             c(2000,233,2532,2533,1902,2100,2530,2531,900,700,2507,380,381,382,383,369,371,370,372)) %>%
#     dplyr::select(tag,  tagstation, libelle, unite, tablehisto) %>%     
#     dplyr::mutate(tablehisto = tolower(tablehisto)) %>% clipr::write_clip(object_type="table")

library(readxl)
isac <- read_excel("data-raw/variables.xlsx", sheet= "isac")
isac$isac <- as.logical(isac$isac)
isac$pluvio <- as.logical(isac$pluvio)
str(isac)
usethis::use_data(isac, overwrite = TRUE)

