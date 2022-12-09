
#' Generic function for loaddb
#' @keywords internal
setGeneric("loaddb",def=function(objet,...) standardGeneric("loaddb"))

#' Methode loaddb
#' 
#' La méthode loaddb permet d'aller chercher les données après avoir installé le driver odbc et avoir appellé la bonne chaine de connection (il doit y avoir une variable nommée sivacon dans le workspace)
#' @param objet, un objet de classe tablesiva
#' @param con, une connexion pool
#' @importFrom stringr str_c
#' @return Un object de classe tablesiva
#' @export
setMethod(
  "loaddb",
  signature = signature("tablesiva"),
  definition = function(objet, con) {

    # si il y a un @tag non NA, vérification qu'il est bien dans les tag de la table
    if (!is.na(objet@tag)) {     
     sql <-
         paste0("SELECT distinct tag FROM archive_IAV.", objet@table)
    tag <- DBI::dbGetQuery(conn=con,sql)
      if (!objet@tag %in% tag$tag)
        stop("le tag n'existe pas dans la table")
    }
    selectsiva <- stringr::str_c("SELECT * FROM archive_IAV.", objet@table)
    if (!is.na(objet@tag)) {
      wheresiva <-
          stringr::str_c(
          " WHERE Horodate>'",
          objet@debut,
          "' AND Horodate<'",
          objet@fin,
          "' AND tag=",
          objet@tag
        )
    } else {
      wheresiva <-
          stringr::str_c(" WHERE Horodate>'",
              objet@debut,
              "' AND Horodate<'",
              objet@fin,
              "'")
    }
    orderbysiva = " ORDER BY HoroDate"
    sql <- stringr::str_c(selectsiva, wheresiva, orderbysiva)
    objet@rawdata <- DBI::dbGetQuery(conn=con, statement = sql)
    if (nrow(objet@rawdata) == 0) 
      warning(paste("no data for", objet@table))
    colnames(objet@rawdata)[3] <- objet@nom
    return(objet)
  }
)







