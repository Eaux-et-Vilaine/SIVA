
#' @noRd
validity_bilansiva = function(object)
{
  rep1 = all(
      length(object@tables) == length(object@noms),
      length(object@daterondes) == length(object@noms),
      length(object@tags) == length(object@noms)
  )
  comment1 = "les attributs tables, noms , daterondes et tags doivent avoir la m\u00eame longeur"
  rep2 = all(object@daterondes %in% c("constant", "linear", "none"))
  comment2 = "les valeurs pour daterondes sont constant,linear,ou none"
  rep3 = object@fin > object@debut
  comment3 = "la date de fin doit d\u00e9passer la date de d\u00e9but"
  return(ifelse(rep1 &
              rep2 &
              rep3 , TRUE, c(comment1, comment2, comment3)[!c(rep1, rep2, rep3)]))
}




#' @title Class bilanSiva
#' Cette classe utilise TableSiva pour récupérer les infos sur plusieurs tables
#' @slot tables = noms des la tables a aller chercher
#' @slot nom = noms des la colonnes
#' @slot daterondes = traitement par la methode datesrondes, soit "constant" pour ouvertures en tout ou rien des vannes, soit "linear" pour des données continues, soit "none" pour pas de traitement
#' @slot tags = si plusieurs variables sont dans la même table il faut donner leur tag pour les différentier
#' @slot debut = date de début
#' @slot fin = date de fin
#' @note Cette classe teste que les longueurs des champs nom, daterondes,et table sont égales
#' @export
setClass(Class="bilansiva",representation=
        representation(tables="character",
            noms="character",
            daterondes="character",
            tags="integer",
            debut="POSIXct",
            fin="POSIXct",
            bilandata="data.frame") ,
    prototype=prototype(tables=character(),
        noms=character(),
        daterondes=character(),
        tags=integer(),
        debut=as.POSIXct(as.Date("2012-10-25")),
        fin=as.POSIXct(as.Date(Sys.time())),
        bilandata= data.frame()
    ),
    validity=validity_bilansiva
)
