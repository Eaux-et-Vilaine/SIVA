#' Chargement des donnees de niveaux
#' 
#' Il faut avoir configuré la connexion à la base, les données de niveau peuvent être mise à jour dans niveau voir variable.R
#'
#' @param debut La date de début format POSIXct ou character "%Y-%m-%d %H:%M:%S
#' @param fin La fin format POSIXct ou character "%Y-%m-%d %H:%M:%S
#' @param tags les tags des données à charger
#' @param con Une connexion pool
#' @return Le tableau de données de niveaux
#' @export
#' @importFrom lubridate is.POSIXct
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
  if (!is.POSIXct(debut)) debut <- as.POSIXct(strptime(debut, format = "%Y-%m-%d %H:%M:%S"))
  if (!is.POSIXct(fin)) fin <-as.POSIXct(strptime(fin, format = "%Y-%m-%d %H:%M:%S"))
  missing <- setdiff(tags, niveau$tag)
  if (length(missing) >0) stop(sprintf("Element(s) %s non presents, verifier tags", paste(missing,collapse=", ")))
  niveau <- SIVA::niveau
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