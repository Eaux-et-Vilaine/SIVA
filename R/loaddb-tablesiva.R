
#' @nord 
setGeneric("loaddb",def=function(objet,...) standardGeneric("loaddb"))

#' Methode loaddb
#' 
#' La méthode loaddb permet d'aller chercher les données après avoir installé le driver odbc et avoir appellé la bonne chaine de connection (il doit y avoir une variable nommée sivacon dans le workspace)
#' @param objet, un objet de classe tablesiva
#' @importFrom stringr str_c
#' @return Un object de classe tablesiva
#' @exportMethod 
#' @examples
#' # sur mon ordi j'ai des mots de passe chargés au démarrage à partir de Rprofile.site
#' # Ici test si existent et si oui il faut le main password pour les decrypter, sinon il
#' # faut les entrer après un prompt du programme. 
#' if (interactive()){
#' if (!exists("mainpass")) mainpass <- getPass::getPass(msg = "main password")
#' if (!exists("hostmysql")) {
#'   hostmysql. <- getPass::getPass(msg = "Saisir host")
#'   # ci dessous pour ne pas redemander au prochain tour
#'   hostmysql <- encrypt_string(string = hostmysql., key = mainpass)
#' } else {
#'   hostmysql. <- decrypt_string(string = hostmysql, key = mainpass)
#' }
#' if (!exists("pwdmysql")) {
#'   pwdmysql. <- getPass::getPass(msg = "Saisir password")
#'   pwdmysql <- encrypt_string(string = pwdmysql., key = mainpass)
#' }  else {
#'   # pass should be loaded
#'   pwdmysql. <- decrypt_string(string = pwdmysql, key = mainpass)
#' }
#' if (!exists("umysql")) {
#'   umysql. <- getPass::getPass(msg = "Saisir user")
#'   umysql <- encrypt_string(string = umysql., key = mainpass)
#' } else {
#'   umysql. <- decrypt_string(string = umysql, key = mainpass)
#' }
#' 
#' options(stacomiR.dbname="archive_IAV", # TODO not used....
#'         stacomiR.host = hostmysql.,
#'         stacomiR.password = pwdmysql.,
#'         stacomiR.user = umysql.,
#'         stacomiR.ODBClink = "archive_IAV")
#' 
#' tablesiva <-
#'   new(
#'     "tablesiva",
#'     debut = as.POSIXct(as.Date("2021-10-25")),
#'     fin =  as.POSIXct(as.Date("2021-12-01")),
#'     table = "b_barrage_volet4_hauteur",
#'     nom = "volet4"
#'   )
#' loaddb(tablesiva)
#' }
setMethod(
  "loaddb",
  signature = signature("tablesiva"),
  definition = function(objet) {
    
    sivacon <-c(getOption("stacomiR.ODBClink"),
                getOption("stacomiR.user"),
                getOption("stacomiR.password")
    )
    
    # si il y a un @tag non NA, vérification qu'il est bien dans les tag de la table
    if (!is.na(objet@tag)) {
      requete <- new("RequeteODBCwhere")
      requete@baseODBC <- sivacon
      requete@select <-
        str_c("SELECT distinct tag FROM archive_IAV.", objet@table)
      requete <- connect(requete)
      tag <- requete@query
      if (!objet@tag %in% tag$tag)
        stop("le tag n'existe pas dans la table")
    }
    requete <- new("RequeteODBCwhere")
    requete@baseODBC <- sivacon
    requete@select <- stringr::str_c("SELECT * FROM archive_IAV.", objet@table)
    if (!is.na(objet@tag)) {
      requete@where <-
        str_c(
          "WHERE Horodate>'",
          objet@debut,
          "' AND Horodate<'",
          objet@fin,
          "' AND tag=",
          objet@tag
        )
    } else {
      requete@where <-
        str_c("WHERE Horodate>'",
              objet@debut,
              "' AND Horodate<'",
              objet@fin,
              "'")
    }
    requete@order_by = "ORDER BY HoroDate"
    
    requete <- connect(requete)
    objet@rawdata <- requete@query
    if (nrow(objet@rawdata) == 0)
      warning(paste("no data for", objet@table))
    colnames(objet@rawdata)[3] <- objet@nom
    return(objet)
  }
)







