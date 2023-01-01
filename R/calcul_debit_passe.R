#' @title Calcul du débit de la passe
#' @description Calcule le débit de la passe à partir des types de fonctionnement du DF et du coefficient de 
#' noyage de la vanne aval
#' @param list_fonct_pass Une liste contenant un data.frame dat détaillant 
#' le foncionnement de la passe par pas de temps de 10 minutes (voir fonction calc_fonct_passe) 
#' @return list_fonct_pass avec calcul du débit dans la passe
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #calcul_debit_passe(ll)
#'  }
#' }
#' @export
calcul_debit_passe <- function(list_fonct_pass){
  ###########################
  # Calcul de debit sur la passe
  ###########################
  dat <- list_fonct_pass$dat
  dat$delta_sas_vanne=dat$niveau_sas_passe-dat$hauteur_vanne_mer
  
  ################################################
  # deversoir denoye si vanne mer > niveau mer
  ################################################
  
  reindex <- (dat$hauteur_vanne_mer>dat$niveau_mer_passe)&
      (dat$per_tar_code==1|dat$per_tar_code==4)
  reindex[is.na(reindex)]=FALSE
  dat$debit_passe_recalcule <- NA
  dat$debit_passe_recalcule[reindex]=0.4*1.5*dat$delta_sas_vanne[reindex]^1.5*sqrt(2*9.81)
  ## not run =commentaire ### 0.3 coeff de debit /# 1.5 largeur de la vanne aval
  
  ################################################
  # deversoir noye
  ################################################
  ## not run =commentaire ### deltasasmer>1/10 *deltasasvanne  (hauteur d'eau sur la vanne <3 m pour 30 cm <1 m pour 10 cm
  ## not run =commentaire ###coeffnoyage<0.9
  reindex1 <- (dat$hauteur_vanne_mer<dat$niveau_mer_passe)&(dat$per_tar_code==1|dat$per_tar_code==4)
  reindex1[is.na(reindex1)] <- FALSE
  dat$coeff_noyage <- (dat$delta_sas_vanne-dat$delta_sas_mer)/dat$delta_sas_vanne
  dat$coeff_debit <- (1-dat$coeff_noyage^1.5)^0.385
  dat$debit_passe_recalcule <- NA
  dat$debit_passe_recalcule[reindex1] <- 
      dat$coeff_debit[reindex1]*0.4*1.5*dat$delta_sas_vanne[reindex1]^1.5*sqrt(2*9.81)
  test_coeff_noyage<-dat$coeff_noyage[reindex1]
  ## not run =commentaire ### la formule fonctionne normalement pour un coefficient de noyage limite
  cat(paste(
          "La formule de calcul de debit fonctionne normalement pour un coefficient de noyage limite",
          "il existe ",
          length(test_coeff_noyage[test_coeff_noyage>0.9]),
          "occurences de coeff > 0.9 sur un total de",
          length(test_coeff_noyage),
          "\n"))

  ################################################
  # passe fermee
  ################################################
  dat$debit_passe_recalcule[dat$per_tar_code==2|dat$per_tar_code==3]=0
  list_fonct_pass$dat <- dat
  list_fonct_pass$coeff_noyage_sup_0.9 <- length(test_coeff_noyage[test_coeff_noyage>0.9])
  return(list_fonct_pass)
}