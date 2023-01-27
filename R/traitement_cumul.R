#' Traitement des données par exemple en sortie de load_isac
#' 
#' traitement des cumuls
#' @note Pour les données de débits utiliser la fonction traitement_SIVA
#' @param dat Un tableau de données, les données avec cumul doivent avoir 
#' pattern dans leur nom
#' @param pattern La chaine de caractère identifiant les cumuls, défaut "cumul"
#' @seealso  traitement_SIVA pour les données de débit du barrage.
#' @return Un tableau de données avec diff pour les totaux
#' 
#' @export
traitement_cumul <- function(dat, pattern="cumul") {
  # avant 2016 les variables n'existent pas, pb de "tags"
  if (length(pattern)!=1) stop("length de pattern doit \u00eatre de 1")
  # colonnes contenant les cumuls
  
  totcol <- grep(pattern, colnames(dat))
  if (length(totcol)==0) stop("Pas de colonne selectionnees, mauvais pattern ?")
  volumes <- dat[, totcol, drop=FALSE]
  volumes[2:nrow(volumes), ] <-
    volumes[2:nrow(volumes), ] - volumes[1:(nrow(volumes) - 1), ]
  volumes <- volumes[-1, ]
  volumes[volumes < 0] <- NA
  dat[2:nrow(dat), totcol] <- volumes
  dat[1, totcol] <- NA
   return(dat)
}
