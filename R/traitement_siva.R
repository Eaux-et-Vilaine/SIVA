#' Traitement des données en sortie de SIVA
#' 
#' recalcule des données des compteurs, enlève les valeurs abérrantes des débits
#' des siphons. Les données doivent avoir 'tot_....' en entête, toutes, les colonnes
#' contenant 'tot' font l'objet d'une différence entre les valeurs des lignes 2:n et les 
#' lignes 1:n-1, les données de la première ligne sont NA
#' Pour les données des Siphons debit_siphon_1, et debit_siphon_2 les débits > 3.8 
#' sont ramenés à 3.8
#' 
#' @param dat Un tableau de données contenant des données totalisées 
#' par exemple chargées par la fonction `load_debit_barrage`
#' @return Un tableau de données avec diff pour les totaux, 
#' tot_vol_vanne, tot_vol_passe, tot_vol_siphon, tot_vol_volet, les débits
#' de siphon corrigés, et une colonne tot_vol= volume total journalier
#' au barrage.
#' 
#' @export
#' @examples
#' # voir example-bilansiva-debit pour le chargement des données de 2020 dans SIVA
#' load(system.file("rawdata2020.Rdata", package="SIVA"))
#' plot(rawdata2020$tot_vol_vanne)
#' # les totaliseurs sont remis à plat
#' cordata2020 <- traitement_siva(dat=rawdata2020)
#' plot(cordata2020$tot_vol_vanne)
traitement_siva <- function(dat) {
  # avant 2016 les variables n'existent pas, pb de "tags"
  if (min(as.numeric(dat$horodate)) < 1451607000)	{
    dat <- dat[, -grep("Tag", colnames(dat))]
  }
  # colonnes contenant les cumuls
  totcol <- grep("tot", colnames(dat))
  volumes <- dat[, totcol]
  volumes[2:nrow(volumes), ] <-
    volumes[2:nrow(volumes), ] - volumes[1:(nrow(volumes) - 1), ]
  volumes <- volumes[-1, ]
  volumes[volumes < 0] <- NA
  dat[2:nrow(dat), totcol] <- volumes
  dat[1, totcol] <- NA
  dat[dat$tot_vol_vanne > 1e5 &
        !is.na(dat$tot_vol_vanne), "tot_vol_vanne"] <- NA
  dat[dat$tot_vol_passe > 1e5 &
        !is.na(dat$tot_vol_passe), "tot_vol_passe"] <- NA
  dat[dat$tot_vol_siphon > 1e5 &
        !is.na(dat$tot_vol_siphon), "tot_vol_siphon"] <- NA
  dat[dat$tot_vol_volet > 60000 &
        !is.na(dat$tot_vol_volet), "tot_vol_volet"] <- NA
  dat$tot_vol_siphon[(dat$tot_vol_siphon / 600) > 2 * 3.8 &
                       !is.na(dat$tot_vol_siphon)] <- NA
  dat$debit_siphon_1[dat$debit_siphon_1 > 3.8] <- NA
  dat$debit_siphon_2[dat$debit_siphon_2 > 3.8] <- NA
  dat$tot_vol = rowSums(dat[, totcol])
  return(dat)
  
}
