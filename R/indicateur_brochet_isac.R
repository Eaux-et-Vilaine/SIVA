#' Analyse de l'indicateur brochet sur l'Isac
#' 
#' note :si on est pas dans la bonne période, pas de calcul du niveau ponte, on
#' prend 2.30 comme référence....)
#' 
#' @param dat Un tableau de données, chargé par load_isac et traitement_debit_isac
#' @param h_favorable Le niveau favorable pour la reproduction
#' @param mois_debut Le mois avant lequel la repro s'effectue et après lequel l'émergence commence
#' @param mois_fin Le mois de fin de l'émergence (inclus)
#' @param niveau_marais Le niveau dans les marais, defaut "isac_niveau_marais"
#' voir le jeu de données isac pour les codes possibles
#' @return Ub vecteur (factor) des types pour le brochet
#' 
#' @export
indicateur_brochet_isac <- function(dat,
                                    h_favorable = 2.30,
                                    h_moyen =2.10,
                                    mois_debut = "2",
                                    mois_fin = "4",  
                                    niveau_marais="isac_niveau_marais"
){
  niveau_marais <- dat[,niveau_marais]
  # repro en janvier - février
  periode_repro <- lubridate::month(dat$horodate)<=as.numeric(mois_debut)
  niveau_ponte <- mean(niveau_marais[
    lubridate::month(dat$horodate)<=as.numeric(mois_debut)], na.rm=T)
  #init
  brochet <- rep("6-inconnu/ hors période", length(niveau_marais))
  # periode de repro
  brochet[periode_repro] <- case_when(
    niveau_marais[periode_repro]<h_moyen ~ "2-repro-mauvais",
    niveau_marais[periode_repro]>=h_moyen & 
      niveau_marais[periode_repro]<h_favorable  ~ "1-repro-moyen",
    niveau_marais[periode_repro]>=h_favorable ~ "0-repro-bon",
    is.na(niveau_marais[periode_repro]) ~ "6-inconnu/ hors période")
  
  # periode alevinage
  if (is.na(niveau_ponte))  niveau_ponte <- 2.30
  periode_alevins <- lubridate::month(dat$horodate)>as.numeric(mois_debut) &
    lubridate::month(dat$horodate)<=as.numeric(mois_fin)
  brochet[periode_alevins] <- case_when(
    niveau_marais[periode_alevins]>=niveau_ponte ~ "3-emergence-bon",
    niveau_marais[periode_alevins]<niveau_ponte & niveau_marais[periode_alevins]>=niveau_ponte -10 ~ "4-emergence-moyen",
    niveau_marais[periode_alevins]<niveau_ponte -10 ~ "5-emergence-mauvais",
    is.na(niveau_marais[periode_alevins]) ~ "6-inconnu/ hors période")
  return(as.factor(brochet))
}