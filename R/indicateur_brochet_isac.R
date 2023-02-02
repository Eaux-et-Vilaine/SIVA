#' Analyse de l'indicateur brochet sur l'Isac
#' 
#' note :si on est pas dans la bonne période, pas de calcul du niveau ponte, on
#' prend 2.30 comme référence....)
#' 
#' @param dat Un tableau de données, chargé par load_isac et traitement_debit_isac.
#' @param h_favorable Le niveau favorable pour la reproduction.
#' @param h_moyen Hauteur en dessus de laquelle la reproduction est considérée comme 
#' moyenne et et en dessous duquel elle est considérée comme mauvaise.
#' @param debut_ponte Date de début de ponte defaut "25/02"
#' @param fin_ponte Date de fin de ponte et début de l'émergence defaut "15/03"
#' @param fin_emergence La date de fin de l'emergence (exclue) defaut "01/05".
#' @param niveau_marais Le nom de la variable de niveau dans les marais, defaut "isac_niveau_marais"
#' voir le jeu de données isac pour les codes possibles.
#' @return Ub vecteur (factor) des types pour le brochet
#' 
#' @export
indicateur_brochet_isac <- function(
    dat,
    h_favorable = 2.30,
    h_moyen =2.20,
    debut_ponte = "25/02",
    fin_ponte = "15/03",  
    fin_emergence = "01/05",
    niveau_marais="isac_niveau_marais"
){
  h_marais <- dat[,niveau_marais]
  if (all(is.na(h_marais))) warning(paste("Attention, pas de donnees pour",niveau_marais))
  if (any(is.na(h_marais))) warning(sprintf("Attention, %s donnees manquantes sur %s pour %s",
                                            sum(is.na(h_marais)),length(h_marais),niveau_marais))
  # Calcul de la saison (adaptation à toute les années)
  mois_debut_ponte <- as.integer(strsplit(debut_ponte, split="/")[[1]][2])
  jour_debut_ponte <- as.integer(strsplit(debut_ponte, split="/")[[1]][1])
  mois_fin_ponte <- as.integer(strsplit(fin_ponte, split="/")[[1]][2])
  jour_fin_ponte <- as.integer(strsplit(fin_ponte, split="/")[[1]][1])
  mois_fin_emergence <- as.integer(strsplit(fin_emergence, split="/")[[1]][2])
  jour_fin_emergence <- as.integer(strsplit(fin_emergence, split="/")[[1]][1])
  mois <- as.integer(format(dat$horodate, '%m'))
  jour <- as.integer(format(dat$horodate, '%d'))
  if (mois_fin_ponte>mois_debut_ponte+1) stop("mois_fin_ponte>mois_debut_ponte+1 
            La fonction de calcul des dates de ponte
            doit etre adaptee pour ajouter un mois intermediaire")
  
  periode_repro <- mois == mois_debut_ponte & jour >= jour_debut_ponte |
      mois == mois_fin_ponte & jour < jour_fin_ponte 
  
  periode_emergence <- mois == mois_fin_ponte & jour >= jour_fin_ponte |
      mois == 4 |
      mois == mois_fin_emergence & jour <= jour_fin_ponte 
  
  # le niveau ponte est le niveau moyen pendant la période de reproduction
  
  niveau_ponte <- mean(h_marais[periode_repro], na.rm=T)

  # par defaut
  brochet <- rep("6-inconnu/hors periode", length(h_marais))
  
  # periode de repro
  brochet[periode_repro] <- case_when(
      h_marais[periode_repro]<h_moyen ~ "2-repro-mauvais",
      h_marais[periode_repro]>=h_moyen & 
          h_marais[periode_repro]<h_favorable  ~ "1-repro-moyen",
      h_marais[periode_repro]>=h_favorable ~ "0-repro-bon",
      is.na(h_marais[periode_repro]) ~ "6-inconnu/hors periode")
  
  # periode alevinage
  if (is.na(niveau_ponte))  niveau_ponte <- 2.30
  
  brochet[periode_emergence] <- dplyr::case_when(
      h_marais[periode_emergence] >= niveau_ponte ~ "3-emergence-bon",
      h_marais[periode_emergence] < niveau_ponte & h_marais[periode_emergence]>=niveau_ponte -10 ~ "4-emergence-moyen",
      h_marais[periode_emergence] < niveau_ponte -10 ~ "5-emergence-mauvais",
      is.na(h_marais[periode_emergence]) ~ "6-inconnu/hors periode")
  
  return(as.factor(brochet))
}