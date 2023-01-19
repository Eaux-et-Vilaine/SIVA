#' Chargement des donnees de l'ISac
#'
#' Il faut avoir configuré la connexion à la base, les données de l'Isac peuvent être mise à jour
#'  dans le jeu de donnéees isac voir variable.R. Le programme charge 10 minutes avant la date de début car il doit supprimer
#' la première ligne ou il y a des NA générés par traitement_SIVA
#'
#' @param debut La date de début format POSIXct ou character "%Y-%m-%d %H:%M:%S
#' @param fin La fin format POSIXct ou character "%Y-%m-%d %H:%M:%S
#' @param tags les tags des données à charger
#' @param con Une connexion pool
#' @return Ub tableau de données des débits chargés directement par loaddb, si
#' certaines variables doivent être recalculées, et les variables debit_recalcule et debit_barQ.
#' @export
#' @importFrom lubridate is.POSIXct
#'
#' @examples
#' \dontrun{
#' load_isac(debut = as.POSIXct(strptime("2010-01-01 00:00:00",
#' format = "%Y-%m-%d %H:%M:%S")),
#'    fin = as.POSIXct(strptime("2010-01-10 00:00:00",
#'    format = "%Y-%m-%d %H:%M:%S")),
#'    tags=c(1902, 380)
#'    }
load_isac <- function(debut,
    fin,
    tags,
    con) {
  isac <- SIVA::isac
  if (!is.POSIXct(debut))
    debut <-
        as.POSIXct(strptime(debut, format = "%Y-%m-%d %H:%M:%S"))
  if (!is.POSIXct(fin))
    fin <- as.POSIXct(strptime(fin, format = "%Y-%m-%d %H:%M:%S"))
  # test error in tags
  missing <- setdiff(tags, isac$tag)
  if (length(missing) > 0)
    stop(sprintf(
            "Element(s) %s non presents, verifier tags",
            paste(missing, collapse = ", ")
        ))
  # get full description from tags

  isac_sel <- isac[isac$tag %in% tags,]
  
  bil <- new("bilansiva",
      tables = tolower(isac_sel$tablehisto),
      noms = isac_sel$code,
      tags = as.integer(isac_sel$tag),
      daterondes = rep("constant", nrow(isac_sel)),
      debut = debut,
      fin = fin
  )
  dat <- loaddb(bil,plot=FALSE, con = con)@bilandata
  attributes(dat)$libelle <- isac_sel$libelle
  return(dat)
}