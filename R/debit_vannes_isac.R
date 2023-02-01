#' Fonction de calcul du débit d'une vanne sur le barrage de l'Isac
#' 
#' Fonction de calcul de débit pour une vanne
#'
#' @param hamont Vecteur de données des niveaux amont.
#' @param haval Vecteur de données des niveaux aval.
#' @param hvanne1 Vecteur de données des hauteurs de vanne 1.
#' @param hvanne2 Vecteur de données des hauteurs de vanne 2.
#' @param lvanne2 Largeur de la vanne2 en rive, 4.98
#' @param lvanne1 Larguer de la vanne1 au centre de l'ouvrage 4.81
#' @param g 9.81.
#' @param canal Loi de débit "manning".
#' @param Cma Coefficient canal pour Manning défaut 0.65.
#'
#' @return  Un data frame avec "Q1" le débit calculé sur la vanne 1 en m3/s, 
#' "Q2" le débit calculé sur la vanne 1 en m3/s, Q le débit total typecalc1,
#' et typecalc2 le type de calcul, "vanne fermee", "pas de données", "Manning",
#' "Manning vers marais" cette dernière correspondant à un écoulement depuis la Vilaine
#' vers le marais, "mig" migration au droit de l'ouvrage (bloquee, transparente, difficile) 
#' @export
#' @author Cedric Briand \email{cedric.briand@eaux-et-vilaine.bzh}
#' @examples

#' # Calcul du débit d'une vanne
#' Qva1 <-
#'   debit_vannes_isac(
#'     hamont = dat$isac_amont2,
#'     haval = dat$pontdecran,
#'     hvanne1 = dat$isac_position_vanne_1,
#'     hvanne2 = dat$isac_position_vanne_2
#'   )
#' plot(Qva1[4000:5000,])
#' 
#' 
debit_vannes_isac <- function(hamont, # vecteur
    haval, # vecteur
    hvanne1, # vecteur
    hvanne2, # vecteur
    lvanne2 = 4.98, # largeur de la vanne
    lvanne1 = 4.81,
    g = 9.81,
    canal = "manning", # un seul choix pour l'instant
    Cma = 0.65, #coefficient canal pour manning,
    hradier = 0.28

){
  # test initial	
  if (!(length(haval)==length(hamont)&length(haval)==length(hvanne1)&length(haval)==length(hvanne2))) stop("haval, hmvilaine, hvanne doivent avoir la m\u00eame longueur")
  #initialisation des variables
  res <- data.frame("hamont"=hamont,"haval"=haval,"hvanne1"=hvanne1,"hvanne2"=hvanne2)
  res$Q1 <- rep(NA,length(haval)) 
  res$Q2 <- rep(NA,length(haval))
  res$Q <-  rep(NA,length(haval))
  h1 <- hamont-0.28
  h2 <- haval-0.28
  delta <- hamont-haval # y dans la formule
  res$Q1[hvanne1==0] <- 0
  res$Q2[hvanne2==0] <- 0
  res$typecalc1<-rep(NA,length(haval))
  res$typecalc2<-rep(NA,length(haval))  
  res$typecalc1[hvanne1==0] <- "vanne fermee"
  res$typecalc2[hvanne2==0] <- "vanne fermee"  	
  res$typecalc1[is.na(delta)] <- "pas de donnee"
  res$typecalc2[is.na(delta)] <- "pas de donnee"
# I4- formule de Manning ------------------------------------------------------------------
  
  if (canal=="manning"){
    # Calcul de débit  ----------------------
    # Formule de Manning-Strickler
    # Vanne 1 ----------------

    loicanal1 <- !is.na(delta) & hvanne1>0 & delta>0 & !is.na(hvanne1)
    loicanalinv1 <- !is.na(delta) & hvanne1>0 & delta<0 & !is.na(hvanne1)
    res$typecalc1[loicanal1] <- "manning"
    res$typecalc1[loicanalinv1] <- "manning vers marais"
    res$Q1[loicanal1] <- Cma*(h1[loicanal1])*lvanne1*sqrt(2*g*delta[loicanal1])
    res$Q1[loicanalinv1] <- -Cma*(h2[loicanalinv1])*lvanne1*sqrt(2*g*-delta[loicanalinv1])
    # Vanne 2 ----------------
    loicanal2 <- !is.na(delta) & hvanne2>0 & delta>0 & !is.na(hvanne2)
    loicanalinv2 <- !is.na(delta) & hvanne2>0 & delta<0 & !is.na(hvanne2)
    res$typecalc2[loicanal2] <- "manning"
    res$typecalc2[loicanalinv2] <- "manning vers marais"
    res$Q2[loicanal2] <- Cma*(h1[loicanal2])*lvanne1*sqrt(2*g*delta[loicanal2])
    res$Q2[loicanalinv2] <- -Cma*(h2[loicanalinv2])*lvanne1*sqrt(2*g*-delta[loicanalinv2])
    # Calcul de débit > Isac ----------------------
    res$Q = res$Q1+res$Q2
    res$mig <- "3-bloquee"
    res$mig[res$Q!=0 & !is.na(res$Q) & abs(delta)<0.2 & abs(delta)>0.05 & !is.na(delta)] <- "1-transparente"
    res$mig[res$Q!=0 & !is.na(res$Q) & abs(delta)>=0.2 & abs(delta)<0.5 & !is.na(delta)] <- "2-difficile"
    res$mig[res$Q!=0 & !is.na(res$Q) & abs(delta)<0.05 & !is.na(delta)] <- "4-inf5cm"
    res$mig <- as.factor(res$mig)
  }	else	{			
    stop("canal doit \u00eatre manning")
  } # end canal	
  return(res)
}
