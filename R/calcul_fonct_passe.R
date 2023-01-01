#' @title Calcul du fonctionnement de la passe d'Arzal
#' @description Par défaut la fonction utilise les niveaux mer et Vilaine barrage puis les niveaux passe 
#' quand ces derniers sont manquants. Calcule les périodes continues avec les mêmes paramètres pour per_tar_code
#' à l'aide de la fonction rle
#' @param dat jeu de données contenant les colonnes c("niveau_vilaine_passe","niveau_mer_passe","niveau_vilaine_barrage","niveau_mer_barrage","niveau_sas_passe",
#'			"hauteur_vanne_vilaine","hauteur_vanne_mer")
#' @param per_dis_identifiant Identifiant du dispositif de franchissement, Default: 1
#' @return list with dat=data frame avec les données corrigées et les fonctionnements de la passe (types d'arrêts),
#' t_periodefonctionnement_dis le tableau à réimporter dans stacomi contenant les périodes de fonctionnement des dispositifs
#' @export
#' @importFrom utils head
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #dat <- data_DF_init
#'  #calc_fonct_pass(dat)
#'  }
#' }
#' @rdname calc_fonct_pass

calcul_fonct_pass <- function(dat, per_dis_identifiant=1){
  vvv <- list()
  # checks -------------------------------------------
  
  cols <- c("horodate","niveau_vilaine_passe","niveau_mer_passe","niveau_vilaine_barrage","niveau_mer_barrage","niveau_sas_passe",
      "hauteur_vanne_vilaine","hauteur_vanne_mer")
  test_column_present <- cols %in% colnames(dat)
  if (!all(test_column_present)) stop("Erreur interne, colonnes ",paste(cols[!test_column_present], collapse=",")," manquantes") 
  # teste que les colonnes sont ordonnées
  stopifnot(sort(dat$horodate)==dat$horodate)
  vvv$fonct_pass <- list()
  cat(nrow(dat), 
      "lignes dans le fichier d\'entr\u00e9e \n")
  vvv$fonct_pass$nb_lignes <- nrow(dat)
  
  #   problèmes de codeur sur vanne aval ou le sas (pas possibel d'avoir 2.8 m de charge sur vanne aval)
  
  dat[dat$hauteur_vanne_mer< -1  &
        !is.na(dat$hauteur_vanne_mer) &
        dat$niveau_sas_passe> 1.8 &
        !is.na(dat$niveau_sas_passe)
      ,"niveau_sas_passe"] <- 
    dat[dat$hauteur_vanne_mer< -1  &
          !is.na(dat$hauteur_vanne_mer) &
          !is.na(dat$niveau_sas_passe) &
          dat$niveau_sas_passe> 1.8 
        ,"niveau_mer_passe"]+0.3
  
  # niveau mer barrage ..................
  
  na_niveau_mer_barrage <- is.na(dat$niveau_mer_barrage)
  if (sum(na_niveau_mer_barrage)>0) cat(sum(na_niveau_mer_barrage), "lignes avec valeurs manquantes pour niveau mer \n")
  na_niveau_mer_passe <-  is.na(dat$niveau_mer_passe)
  dat$niveau_mer_barrage[na_niveau_mer_barrage & !na_niveau_mer_passe] <- 
      dat$niveau_mer_passe[na_niveau_mer_barrage & !na_niveau_mer_passe]
  if (sum(na_niveau_mer_barrage & !na_niveau_mer_passe)>0) cat(
        sum(na_niveau_mer_barrage & !na_niveau_mer_passe),
        "lignes niveau mer barrage remplac\u00e9es par niveau mer passe \n")	
  vvv$fonct_pass$niveau_mer_barrage_manquant_rempl <- sum(na_niveau_mer_barrage & !na_niveau_mer_passe)
  na_niveau_mer_barrage <- is.na(dat$niveau_mer_barrage)  # prise en compte des valeurs remplacées
  vvv$fonct_pass$niveau_mer_manquant_final <- sum(na_niveau_mer_barrage)
  
  # niveau vilaine barrage ..................
  
  trop_bas_niveau_vilaine <- dat$niveau_vilaine_barrage< -0.5 & !is.na(dat$niveau_vilaine_barrage)
  dat$niveau_vilaine_barrage[trop_bas_niveau_vilaine] <- NA
  vvv$fonct_pass$trop_bas_niveau_vilaine <- sum(trop_bas_niveau_vilaine)
  na_niveau_vilaine_barrage <- is.na(dat$niveau_vilaine_barrage)
  if (sum(na_niveau_vilaine_barrage)>0) cat(sum(na_niveau_vilaine_barrage), "lignes avec valeurs manquantes pour niveau Vilaine \n")
  na_niveau_vilaine_passe <-  is.na(dat$niveau_vilaine_passe)
  dat$niveau_vilaine_barrage[na_niveau_vilaine_barrage & !na_niveau_vilaine_passe &  dat$niveau_vilaine_passe>= -0.5] <- 
      dat$niveau_vilaine_passe[na_niveau_vilaine_barrage & !na_niveau_vilaine_passe & dat$niveau_vilaine_passe>= -0.5]
  if (sum(na_niveau_vilaine_barrage & !na_niveau_vilaine_passe  &  dat$niveau_vilaine_passe>= -0.5)>0) cat(
        sum(na_niveau_vilaine_barrage & !na_niveau_vilaine_passe  &  dat$niveau_vilaine_passe>= -0.5),
        "lignes niveau Vilaine barrage remplac\u00e9es par niveau Vilaine passe \n")	
  vvv$fonct_pass$niveau_vilaine_barrage_manquant_rempl <- sum(na_niveau_vilaine_barrage & !na_niveau_vilaine_passe)
  na_niveau_vilaine_barrage <- is.na(dat$niveau_vilaine_barrage) # prise en compte des valeurs remplacées
  vvv$fonct_pass$niveau_vilaine_manquant_final <- sum(na_niveau_vilaine_barrage)
  
  
  
  # Fermeture de la vanne amont ...........................
  
  # pb niveau mer trop faible ..................
  
  pb_niveau_mer <- !na_niveau_mer_barrage & dat$niveau_mer_barrage< -3.5
  if (sum(pb_niveau_mer)>0) cat(sum(pb_niveau_mer),"lignes avec niveau mer < -3.5  => corrig\u00e9es \n")
  dat[pb_niveau_mer,"niveau_mer_barrage"] <- -3.5
  vvv$fonct_pass$pb_niveau_mer_inf_3.5 <- sum(pb_niveau_mer)
  # pb niveau sas passe trop faible ou fort ..................
  
  pb_niveau_sas_passe_inf_m1 <- dat$niveau_sas_passe < -1   & !is.na(dat$niveau_sas_passe)
  pb_niveau_sas_passe_sup_2.5 <- dat$niveau_sas_passe > 2.5 & !is.na(dat$niveau_sas_passe)
  if (sum(pb_niveau_sas_passe_inf_m1)>0) cat(sum(pb_niveau_sas_passe_inf_m1),"lignes avec niveau SAS < -1 => corrig\u00e9es \n")
  if (sum(pb_niveau_sas_passe_sup_2.5)>0) cat(sum(pb_niveau_sas_passe_sup_2.5),"lignes avec niveau SAS > 2.5  => corrig\u00e9es \n")
  dat$niveau_sas_passe[pb_niveau_sas_passe_inf_m1] <- -1
  dat$niveau_sas_passe[pb_niveau_sas_passe_sup_2.5] <- 2.5
  vvv$fonct_pass$pb_niveau_sas <- sum(pb_niveau_sas_passe_inf_m1,pb_niveau_sas_passe_sup_2.5)
  # pb hauteur vanne mer trop faible ou fort ..................	
  
  pb_hauteur_vanne_mer_inf_m1.72 <- dat$hauteur_vanne_mer< -1.72   & !is.na(dat$hauteur_vanne_mer)
  pb_hauteur_vanne_mer_sup_2.08 <- dat$hauteur_vanne_mer>2.08 & !is.na(dat$hauteur_vanne_mer)
  if (sum(pb_hauteur_vanne_mer_inf_m1.72)>0) cat(sum(pb_hauteur_vanne_mer_inf_m1.72),"lignes avec niveau vanne mer < -1.72  => corrig\u00e9es \n")
  if (sum(pb_hauteur_vanne_mer_sup_2.08)>0) cat(sum(pb_hauteur_vanne_mer_sup_2.08),"lignes avec niveau vanne mer  > 2.08  => corrig\u00e9es \n")
  
  dat$hauteur_vanne_mer[pb_hauteur_vanne_mer_sup_2.08] <- 2.08
  dat$hauteur_vanne_mer[pb_hauteur_vanne_mer_inf_m1.72] <- 1.72
  vvv$fonct_pass$pb_hauteur_vanne_mer <- sum(pb_hauteur_vanne_mer_sup_2.08,pb_hauteur_vanne_mer_inf_m1.72)
  
  # Initialisation des variables -----------------------------------
  
  dat$delta_vilaine_mer <- dat$niveau_vilaine_barrage - dat$niveau_mer_barrage
  dat$delta_sas_mer <- dat$niveau_sas_passe - dat$niveau_mer_barrage
  dat$per_etat_fonctionnement <- as.integer(NA)
  dat$per_tar_code <- as.integer(NA)
  
  # CAS 1 - si on a pas d'information sur le Niveau Vilaine-----------------
  
  dat[is.na(dat$delta_vilaine_mer) | is.na(dat$hauteur_vanne_vilaine) | is.na(dat$delta_sas_mer) , 
      "per_tar_code"] <- 5
  dat[is.na(dat$delta_vilaine_mer) | is.na(dat$hauteur_vanne_vilaine) | is.na(dat$delta_sas_mer) , 
      "per_etat_fonctionnement"] <- TRUE # temporary
  
  cat(sum(is.na(dat$delta_vilaine_mer) | is.na(dat$hauteur_vanne_vilaine) | is.na(dat$delta_sas_mer)), 
      "lignes avec valeurs manquantes pour le calcul du fonctionnement de la passe \n")
  vvv$fonct_pass$pb_na <- sum(is.na(dat$delta_vilaine_mer) | is.na(dat$hauteur_vanne_vilaine) | is.na(dat$delta_sas_mer))
  
  # CAS 2 quand le delta entre la Vilaine et la mer est de moins de 0.3, la vanne amont est fermée, la passe est fermée
  # Arrêt du dispositif lié au fonctionnement normal du dispositif
  
  dat[dat$delta_vilaine_mer<0.3 & dat$hauteur_vanne_vilaine==-0.2 & is.na(dat$per_etat_fonctionnement), 
      "per_tar_code"] <- 2
  dat[dat$delta_vilaine_mer<0.3 & dat$hauteur_vanne_vilaine==-0.2 & is.na(dat$per_etat_fonctionnement), 
      "per_etat_fonctionnement"] <- FALSE
  
  # CAS 3 la vanne Vilaine est fermée, la passe est fermée mais c'est pas pour une raison de maintenance
  # La vanne aval ou la vanne amont peuvent être fermées
  
  dat[dat$hauteur_vanne_vilaine==-0.2 & is.na(dat$per_etat_fonctionnement), 
      "per_tar_code"] <- 3
  dat[dat$hauteur_vanne_vilaine==-0.2 & is.na(dat$per_etat_fonctionnement), 
      "per_etat_fonctionnement"] <- FALSE
  
  dat[dat$hauteur_vanne_mer==1.72 & is.na(dat$per_etat_fonctionnement), 
      "per_tar_code"] <- 3
  dat[dat$hauteur_vanne_mer==1.72 & is.na(dat$per_etat_fonctionnement), 
      "per_etat_fonctionnement"] <- FALSE
  
  
  # CAS 4 la passe est ouverte mais la chute aval est trop forte
  
  dat[dat$delta_sas_mer>0.5 & is.na(dat$per_etat_fonctionnement), 
      "per_tar_code"] <- 4
  dat[dat$delta_sas_mer>0.5 & is.na(dat$per_etat_fonctionnement), 
      "per_etat_fonctionnement"] <- FALSE
  
  # CAS 5 tous les autres cas la passe fonctionne
  
  dat[is.na(dat$per_etat_fonctionnement) , 
      "per_tar_code"] <- 1
  dat[is.na(dat$per_etat_fonctionnement) , 
      "per_etat_fonctionnement"] <- TRUE
  
  # finally reset NA to missing values
  
  dat[is.na(dat$delta_vilaine_mer) | is.na(dat$hauteur_vanne_vilaine) | is.na(dat$delta_sas_mer) , 
      "per_etat_fonctionnement"] <- NA # temporary
  
  vvv$fonct_pass$table_etat_fonct <- table(dat$per_tar_code)
  
  rle_per_tar_code <- rle(dat$per_tar_code) # run length encoding gives consecutive values
  index <- utils::head(cumsum(rle(dat$per_tar_code)$lengths)+1,-1)  # remove last with head
  t_periodefonctdispositif_per <- 
      data.frame("per_dis_identifiant" = rep(per_dis_identifiant,length(index)-1),
          "per_date_debut"= 	dat$horodate[index[-length(index)]],
          "per_date_fin" = dat$horodate[index[-1]],
          "per_commentaires" = "fonct calcul",
          "per_etat_fonctionnement" = dat$per_etat_fonctionnement[index[-length(index)]],
          "per_tar_code" = dat$per_tar_code[index[-length(index)]])
  cat("Creation de t_periodefonctdispositif_per","nb lignes= ",nrow(t_periodefonctdispositif_per),"\n")
  vvv$fonct_pass$nb_lignes_t_periodefonct <- nrow(t_periodefonctdispositif_per)
  vvv <<- vvv # retroune le résultat de la liste vvv dans l'environnement principal
  return(list(dat=dat,t_periodefonctdispositif_per=t_periodefonctdispositif_per, vvv=vvv))
} # end function calcul_fonc_pass