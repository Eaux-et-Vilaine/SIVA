#' Classe de connexion SIVA
#' 
#' 
#' 
#' @slot table Nom de la table (serie de données) dans SIVA.
#' @slot nom Nom de la série dans le tableau de données.
#' @slot tag Tag dans siva
#' @slot debut Horodate de début.
#' @slot fin Horodate de fin.
#' @slot rawdata Données brutes.
#' @slot corrdata Données corrigées.
#'
#' @return Un objet de classe tablesiva.
#' 
#' @export
setClass(
  Class = "tablesiva",
  representation =
    representation(
      table = "character",
      nom = "character",
      tag = "integer",
      debut = "POSIXct",
      fin = "POSIXct",
      rawdata = "data.frame",
      corrdata = "data.frame"
    ) ,
  prototype = prototype(
    table = character(length = 1),
    nom = character(length = 1),
    tag = as.integer(NA),
    debut = as.POSIXct(as.Date("2012-10-25 00:00:00")),
    fin = as.POSIXct(as.Date(Sys.time()))
  )
)


