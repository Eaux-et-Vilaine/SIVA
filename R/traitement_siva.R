#' Traitement des données en sortie de SIVA
#' 
#' recalcule des données des compteurs, enlève les valeurs abérrantes des débits
#' des siphons. Les données doivent avoir 'tot_....' en entête, toutes, les colonnes
#' contenant 'tot' font l'objet d'une différence entre les valeurs des lignes 2:n et les 
#' lignes 1:n-1, les données de la première ligne sont NA.
#' Les volumes passe > 1500 
#' Pour les données des Siphons debit_siphon_1, et debit_siphon_2 les débits > 3.8 
#' sont ramenés à 3.8. Les volumes du siphon sont des valeurs .... journalières,
#' pour plus de cohérence elles sont recalculées à partir des débits des siphons,
#' et réinjectée à la place de tot_vol_siphon.
#' 
#' @param dat Un tableau de données contenant des données totalisées 
#' par exemple chargées par la fonction `load_debit_barrage`
#' @return Un tableau de données avec diff pour les totaux, 
#' tot_vol_vanne, tot_vol_passe, tot_vol_siphon, tot_vol_volet, les débits
#' de siphon corrigés, et une colonne tot_vol= volume total journalier
#' au barrage.
#' 
#' @export
#' @seealso traitement_cumul pour les données chargées par une autre fonction
#' que load_debit_barrage
#' @examples
#' # voir example-bilansiva-debit pour le chargement des données de 2020 dans SIVA
#' rawdata2020 <- SIVA::rawdata2020
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
  
  dat <- traitement_cumul(dat, pattern = "tot")
  totcol <- grep("tot", colnames(dat))
  # vanne ----------------------------------------
  
  test_vanne <- dat$tot_vol_vanne > 480000 &
    !is.na(dat$tot_vol_vanne)
  ct_vanne <- sum(test_vanne)
  if (ct_vanne >0) {
    warning(sprintf("Volume vanne, %s valeurs au dessus de 480000 m3 par 10 min (800 m3/s), pas de traitement mais \u00e7a vaut le coup de regarder ",ct_vanne))
  }
  
  # passe ----------------------------------------
  
  # les volumes sont pourris
  test_passe <- dat$debit_passe > 5 &
    !is.na(dat$debit_passe)
  ct_passe <- sum(test_passe)
  if (ct_passe >0) {
  dat[ct_passe, "debit_passe"] <- NA
  warning(sprintf("D\u00e9bit passe, %s valeurs au dessus de 5 m3/s) transform\u00e9es en NA",ct_passe))
  }
  dat$tot_vol_passe <- dat$debit_passe*600
  
  # volets ----------------------------------------
  
  test_volet <- dat$tot_vol_volet > 80000 &
    !is.na(dat$tot_vol_volet)
  ct_volet <- sum(test_volet)
  if (ct_volet >0) {
    dat[ct_volet, "tot_vol_volet"] <- NA
    warning(sprintf("Volume volet, %s valeurs au dessus de 80 000 m3 par 10 min transform\u00e9es en NA (133 m3/s)",ct_volet))
  }
  
  # Siphon ----------------------------------------

  # les volumes du siphon sont des volumes à la journée
  # je les remplace à partir du calcul des débits du siphon
  
  test_siphon1 <- dat$debit_siphon_1 > 3.8 &
    !is.na(dat$debit_siphon_1)
  ct_siphon1 <- sum(test_siphon1)
  if (ct_siphon1 >0) {
    dat$debit_siphon_1[dat$debit_siphon_1 > 3.8] <- NA
    warning(sprintf("D\u00e9bit siphon1, %s valeurs au dessus de 3.8 m3/s transform\u00e9es en NA ",ct_siphon1))
  }
  
  test_siphon2 <- dat$debit_siphon_2 > 3.8 &
    !is.na(dat$debit_siphon_2)
  ct_siphon2 <- sum(test_siphon2)
  if (ct_siphon2 >0) {
    dat$debit_siphon_2[dat$debit_siphon_2 > 3.8] <- NA
    warning(sprintf("D\u00e9bit siphon2, %s valeurs au dessus de 3.8 m3/s transform\u00e9es en NA ",ct_siphon2))
  }
 
  dat$tot_vol_siphon <- rowSums(dat[,c("debit_siphon_1","debit_siphon_2")]*600,na.rm=TRUE)

  dat$tot_vol = rowSums(dat[, totcol],na.rm = TRUE)
  return(dat)
}
