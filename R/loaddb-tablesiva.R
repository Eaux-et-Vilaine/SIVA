
#' Generic function for loaddb
#' @keywords internal
setGeneric("loaddb",def=function(objet,...) standardGeneric("loaddb"))

#' Methode loaddb
#' 
#' La méthode loaddb permet d'aller chercher les données 
#' @param objet, un objet de classe tablesiva
#' @param con, une connexion pool
#' @param checktag, faut il faire un check de l'existence d'un tag dans la table ?
#' @importFrom stringr str_c
#' @return Un object de classe tablesiva
#' @export
setMethod(
  "loaddb",
  signature = signature("tablesiva"),
  definition = function(objet, con, checktag = FALSE) {
    if (checktag) {
      # si il y a un @tag non NA, vérification qu'il est bien dans les tag de la table
      if (!is.na(objet@tag)) {     
        sql <-
          paste0("SELECT distinct tag FROM archive_IAV.", objet@table)
        tag <- DBI::dbGetQuery(conn=con,sql)
        if (!objet@tag %in% tag$tag)
          stop("le tag n'existe pas dans la table")
      }
    }
    selectsiva <- stringr::str_c("SELECT horodate,valeur FROM archive_IAV.", objet@table)
    if (!is.na(objet@tag)) {
      wheresiva <-
        stringr::str_c(
          " WHERE horodate>='",
          objet@debut,
          "' AND horodate<='",
          objet@fin,
          "' AND tag=",
          objet@tag
        )
    } else {
      wheresiva <-
        stringr::str_c(" WHERE horodate>'",
                       objet@debut,
                       "' AND horodate<'",
                       objet@fin,
                       "'")
    }
    orderbysiva = " ORDER BY horodate"
    sql <- stringr::str_c(selectsiva, wheresiva, orderbysiva)
    objet@rawdata <- DBI::dbGetQuery(conn=con, statement = sql)
    cat(sprintf("Table %s(%s:%s), chargement de %s lignes \n",objet@nom,objet@table,objet@tag,nrow(objet@rawdata)))
    if (nrow(objet@rawdata) == 0) 
      warning(paste("no data for", objet@table,"\n"))
    colnames(objet@rawdata)[2] <- objet@nom
    return(objet)
  }
)







