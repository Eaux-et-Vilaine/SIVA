#' Traitement des données de l'isac
#' 
#' traitement des cumuls + correction
#' @note Les données sont corrigées comme suit :
#' les données avec des données de débit pompe >0 et vanne ouvertes =>0,
#' les données avec des débits horaires supérieur à 40 sont mis en NA
#' la consommation électrique des pompes est calculée à partir de la puissance 
#' moyenne 50 kW. La conso carbone en utilisant un coefficient moyen.
#' 
#' @param dat Un tableau de données de l'isac chargées avec \code{\link{load_isac}}
#' @param cout_elec Cout de l'electricité Kwh defaut 0.1740
#' @param puissance_moy_pompe Puissance moyenne de la pompe de l'Isac defaut 50 KW
#' @param cout_carbone_kwh Cout en kg de CO2 du kilowatt here défaut 0.1
#' @return Un tableau de données avec les donnees d'entrée et
#' \item{h}{le delta amont aval en m (les débits pompe sont calculés avec -h).}
#' \item{t}{ le temps de fonctionnement cumulé des pompes ... corrigé en enlevant
#' les fonctionnements vannes ouvertes et les valeurs > 40 min par 10 min.}
#' \item{cout_euros}{ Le coût en euros de fonctionnement (juste la conso électrique, ne prend pas 
#'  en compte de le coût de l'abonnement).}
#' \item{cout_carbone}{ Le coût carbone de fonctionnement des pompes basé sur le cout carbone
#' du KWh en France.}
#' 
#' @export
traitement_isac <- function(dat, cout_elec = 0.1740, puissance_moy_pompe =50, cout_carbone_kwh=0.1) {
  # fonctions utiles -------------------------------------------------------
  cout <- function(t, v=cout_elec, P=puissance_moy_pompe){cout=t*v*P }
  carbon = function(t){cout_carbone_kwh*t*50}
  debit <-  function(h, t){
    Q <- 1318 - 131.8*h
    Q <- Q/1000
    Q[t==0 & !is.na(t)] <- 0
    Q[is.na(t)] <- NA
    return(Q)
  }
  
  
  # Calculs ---------------------------------------------------------------
  
  dat <- traitement_cumul(dat)
  if(!"isac_fonctionnement_cumul_p1" %in% colnames(dat))
    stop("isac_fonctionnement_cumul_p1 doit etre dans le jeu de donnees")
  if(!"isac_fonctionnement_cumul_p3" %in% colnames(dat))
    stop("isac_fonctionnement_cumul_p3 doit etre dans le jeu de donnees")
  if(!all(c("isac_amont2","pontdecran") %in% colnames(dat))){
    cat("isac_amont2 et / ou pontdecran ne sont pas dans le jeu de donnees => pas
        de calcul de debit")
    h <- rep(NA, nrow(dat))
  } else {
    h <- dat$isac_amont2 - dat$pontdecran
  }
  
  # si une des vannes est ouverte alors les pompes sont arretees
  dat$isac_fonctionnement_cumul_p1[dat$isac_fonctionnement_cumul_p1>0 & 
                                     !is.na(dat$isac_fonctionnement_cumul_p1) &
                                     (dat$isac_position_vanne_1>0 |
                                        dat$isac_position_vanne_2>0)] <-NA
  dat$isac_fonctionnement_cumul_p3[dat$isac_fonctionnement_cumul_p3>0 & 
                                     !is.na(dat$isac_fonctionnement_cumul_p3) &
                                     (dat$isac_position_vanne_1>0 |
                                        dat$isac_position_vanne_2>0)] <-NA
  # les valeurs superieures à 40 sont considerees fausses apres examen graphique
  dat$isac_fonctionnement_cumul_p1[dat$isac_fonctionnement_cumul_p1*60>40 ] <-NA
  dat$isac_fonctionnement_cumul_p3[dat$isac_fonctionnement_cumul_p3*60>40 ] <-NA
  t <- rowSums(dat[,c("isac_fonctionnement_cumul_p1","isac_fonctionnement_cumul_p3")], na.rm=T)


  # temps en heure 
  dat$h <- h
  dat$t <- t
  dat$cout_euros <-  cout(t)
  dat$cout_carbone <- carbon(t)
  dat$Q_pompes <- debit(-h, t)
  
  return(dat)
}