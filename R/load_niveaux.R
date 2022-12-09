#' Chargement des donnees de niveaux
#' 
#' Il faut avoir configuré la connexion à la base
#'
#' @param debut La date de début format POSIXct
#' @param fin La fin
#' @param tags les tags des données à charger
#' @param con Une connexion pool
#' @return Le tableau de données de niveaux
#' @export
#'
#' @examples
#' \dontrun{
#' load_niveaux(debut = as.POSIXct(strptime("2010-01-01 00:00:00", 
#' format = "%Y-%m-%d %H:%M:%S")),
#'    fin = as.POSIXct(strptime("2010-01-10 00:00:00", 
#'    format = "%Y-%m-%d %H:%M:%S")),
#'    tags=c(2507,2508,2100)
#'    }
load_niveaux <- function(debut, 
                         fin,
                         tags,
                         con){
  if (!is.POSIXct(debut)) stop("La date de debut doit etre au format POSIXct")
  if (!is.POSIXct(fin)) stop("La date de fin doit etre au format POSIXct")
  missing <- setdiff(tags, niveau$tag)
  if (length(missing) >0) stop(sprintf("Element(s) %s non presents, verifier tags", paste(missing,collapse=", ")))
  niveau_sel <- niveau[niveau$tag %in% tags,]

  bil <- new("bilansiva",
           tables= tolower(niveau_sel$tablehisto),
           noms =niveau_sel$code,
           tags=as.integer(niveau_sel$tag),
           daterondes=rep("constant", nrow(niveau_sel)),
           debut=debut,
           fin=fin
           )
  dat <- loaddb(bil,plot=FALSE, con = con)@bilandata
  attributes(dat)$libelle <- niveau_sel$libelle
  return(dat)
}