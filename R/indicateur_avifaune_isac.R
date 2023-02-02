#' Analyse de l'indicateur avifaune sur l'Isac
#' 
#' @param dat Un tableau de données, chargé par load_isac et traitement_debit_isac
#' @param h_favorable Le niveau favorable à l'accueil d'oiseaux hivernants sur
#' l'Isac
#' @param h_moyen Le niveau en dessous duquel l'accueil des oiseaux hivernants 
#' devient mauvais
#' @param mois_debut Le mois de début de l'accueil de l'avifaune (inclus)
#' @param mois_fin Le mois de fin de l'accueil de l'avifaune (inclus)
#' @param niveau_marais Le niveau dans les marais, defaut "isac_niveau_marais"
#' voir le jeu de données isac pour les codes possibles
#' @return Ub vecteur (factor) des types pour l'avifaune
#' @importFrom dplyr case_when
#' @export
indicateur_avifaune_isac <- function(dat,
                                     h_favorable = 2.40,
                                     h_moyen = 2.20,
                                     mois_debut = "11",
                                     mois_fin = "4",  
                                     niveau_marais="isac_niveau_marais"
){
  
  h_marais <- dat[,niveau_marais]
  if (all(is.na(h_marais))) warning(paste("Attention, pas de donnees pour",niveau_marais))
  if (any(is.na(h_marais))) warning(sprintf("Attention, %s donnees manquantes sur %s pour %s",
                                            sum(is.na(h_marais)),length(h_marais),niveau_marais))
  avifaune <- dplyr::case_when(
    h_marais>=h_favorable ~ "0-bon",
    h_marais<h_favorable & h_marais>=h_moyen ~ "1-moyen",
    h_marais<h_moyen ~ "2-mauvais",
    is.na(h_marais) ~ "4-inconnu")
  avifaune[lubridate::month(dat$horodate) > as.numeric(mois_fin) &
             lubridate::month(dat$horodate) < as.numeric(mois_debut)]<-"3-hors periode"
  return(as.factor(avifaune))
}
