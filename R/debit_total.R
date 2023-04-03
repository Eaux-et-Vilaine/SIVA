#' Débit de l'ensemble des volets et des vannes du barrage.
#' 
#' Débit total (cette fonction retourne le débit total journalier et la valeur moyenne
#' Pondérée du débit de Cran.) en utilisant les choix IFSW pour les fonctions de débit
#' Briand et Woimant (2015). Le calcul se fait en utilisant canal="ifsw" et loi_orificenoye = "ifws".
#' 
#' 
#'
#' @param param Les paramètres à optimiser (4 paramètres "Cvo","Cvg","Cvgs","Cvw") voir 
#' @param param0 Paramètre sans optim
#' @param dat Tableau de données formaté en entrée, colonnes volet1,volet2,volet3,
#' volet4,volet5,vanne1,vanne2,vanne3,vanne4,vanne5,debit_vilaine_estime,debit_passe,
#' debit_moyen_cran,tot_vol_vanne,tot_vol_passe,tot_vol_siphon,tot_vol_volet,
#' tot_vol_ecluse,niveauvilaine,niveaumer,niveauvilaineb,niveaumerb,debit_siphon_1,
#' debit_siphon_2,debit_vanne1,debit_vanne2,debit_vanne3,debit_vanne4,debit_vanne5,
#' debit_volet1,debit_volet2,debit_volet3,debit_volet4,debit_volet5,tot_vol

#' @note Cette fonction a été utilisée pour optimiser les coefficients de débit, 
#' param est consititué des coefficients optimisés
#' mais il faut les autres qui sont stocké dans param0 pour faire tourner débit_total
#' Ainsi a_optimiser tourne bien sur param mais débit total sur param + les coeff de param0.
#' CHANGEMENT 2016 la référence devient niveaumerb et niveauVilaineb.
#' 
#'
#' @return  Un data frame avec 
#' \item{date}{la date (au format date),}
#' \item{Qva12345}{la somme des débits des vannes 1 à 5,}
#' \item{Qvo12345}{la somme des débits des volets 1 à 5,}
#' \item{tot_vol_vanne}{Le volume total évacué par les vannes,}
#' \item{tot_vol_ecluse}{le volume total évacué par l'écluse,}
#' \item{tot_vol_siphon}{le volume total évacucé par les deux siphons,}
#' \item{tot_vol_passe}{le volume total évacucé par la passe,}
#' \item{debit_moyen_cran}{le débit moyen journalier au pont de Cran}	 
#' @export
debit_total <-
  function(param = c(
    "Cvo" = 0.392,
    "Cvg" = 1.5,
    "Cvgs" = 1.5,
    "Cvw" = 1.5
  ),
  param0,
  dat) {
    if (length(param) < 4)
      param <- c(param, param0[!names(param0) %in% names(param)])
    dat$niveauvilaineb[is.na(dat$niveauvilaineb)] <-
      dat$niveauvilaine[is.na(dat$niveauvilaineb)]
    dat$niveaumerb[is.na(dat$niveaumerb)] <-
      dat$niveaumer[is.na(dat$niveaumerb)]
    Qvo12345 <- debit_volets(
      hvolet1 = dat$volet1,
      hvolet2 = dat$volet2,
      hvolet3 = dat$volet3,
      hvolet4 = dat$volet4,
      hvolet5 = dat$volet5,
      hvilaine = dat$niveauvilaineb,
      hmer = dat$niveaumerb,
      hvanne1 = dat$vanne1,
      hvanne2 = dat$vanne2,
      hvanne3 = dat$vanne3,
      hvanne4 = dat$vanne4,
      hvanne5 = dat$vanne5,
      param["Cvo"]
    )
    Qvo12345$volvoletcalcule <- rowSums(Qvo12345) * 600
    Qva12345 <- debit_vannes(
      horodate = dat$horodate,
      hvanne1 = dat$vanne1,
      hvanne2 = dat$vanne2,
      hvanne3 = dat$vanne3,
      hvanne4 = dat$vanne4,
      hvanne5 = dat$vanne5,
      hvilaine = dat$niveauvilaineb,
      hmer = dat$niveaumerb,
      canal = "ifsw",
      # choix bazin, aubuisson ou horton ou manning ou ifsw
      Cvg = param["Cvg"],
      #coefficient ifsw orifice h1<=1.5 hvanne
      Cvgs = param["Cvgs"],
      # coefficient ifsw h1>1.5 hvanne
      Cvw = param["Cvw"],
      #coefficient ifsw écoulement surface libre (canal)
      loi_orificenoye = "ifws"#"ferrete"
    )
    # bug #23
    # no data for tot_vol_passe no data for tot_vol_siphon historically < 2010
    if (!"tot_vol_passe" %in% colnames(dat)) dat$tot_vol_passe <- dat$debit_passe * 600
    if (!"tot_vol_siphon" %in% colnames(dat)) dat$tot_vol_siphon <- rowSums(dat[,c("debit_siphon_1", "debit_siphon_2")]) * 600
    
    Q12345 <-
      cbind(
        Qva12345,
        Qvo12345,
        "tot_vol_volet" = dat$tot_vol_volet,
        "tot_vol_vanne" = dat$tot_vol_vanne,
        "tot_vol_ecluse" = dat$tot_vol_ecluse,
        "tot_vol_siphon" = dat$tot_vol_siphon,
        "tot_vol_passe" = dat$tot_vol_passe
      )
    Q12345$date = as.Date(Q12345$horodate)
    Q12345$debit_moyen_cran <- dat$debit_moyen_cran
    return(Q12345)
}
