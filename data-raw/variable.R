## code to prepare `variable` dataset goes here
library(stacomirtools)
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



sivacon <- c(
  getOption("stacomiR.ODBClink"),
  getOption("stacomiR.user"),
  getOption("stacomiR.password")
)

requete <- new("RequeteODBC")
requete@baseODBC <- sivacon
requete@sql <-
  stringr::str_c(
    "SELECT  Tag, TagStation, Libelle, MinStation, MaxStation, 
    MinPC, MaxPC, Format, Unite, TableHisto, TableBilan, Categorie, 
    tag_type_domaine, tag_domaine, tag_sous_domaine, tag_bassin, 
    tag_sous_bassin, tag_sous_bassin_loc
FROM SIVA_R.svg2015_variable"
  )
requete <- connect(requete)
variable <- requete@query
usethis::use_data(variable, overwrite = TRUE)
