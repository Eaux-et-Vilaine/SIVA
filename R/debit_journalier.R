



#' Débit journalier du barrage.
#' 
#' Attention la valeur du jour 1 est enlevée à cause des totaliseurs.
#'
#' @param debit_barrage Le jeu de données issu du chargement `load_debit_barrage`.
#' @param type Le type de sortie, soit "recacule", sinon "barrage_volume" ou
#' barrage_debit.
#'
#' @return Un tableau de données de débit journier.
#' Un tibble avec : 
#' 
#' si type = "recacule" :
#' vol_recalcule : le volume recaculé (m3) par les fonctions de débit de SIVA 
#' pour les vannes et les volets, plus le volume des siphons, de la passe,
#' et de l'écluse, issus de la base de données du barrage.
#' debit_moyen_recalcule = vol_recalc / 86400,
#' 
#' si type = barrage_volume : 
#' vol_bar = volume évacué au barrage,
#' vol_vanne = volume évacué par les vannes,
#' vol_volet = id... volets,
#' vol_siphon = id... siphon,
#' vol_ecluse = id... ecluse,
#' vol_passe = id... passe,
#' debit_moyen_vol_bar = vol_bar/86400,
#' ... 
#' 
#' si type = barrage_debit :
#' vol_vanne_barQ = vol vannes à partir des débits,
#' vol_volet_barQ = vol volet à partir des débits,
#' debit_vilaine_estime = ramené au jour, on ne sait pas trop ce que c'est,
#' debit_moyen_cran = débit moyen de Cran calculé comme somme des volumnes
#' instantannés, divisés par 86400,
#' volume_total_barQ = volume total à partir des totaliseurs pour passe, ecluse,
#'  siphon, et à partir du débit moyen 10 min des vannes et des volets,
#' debit_volet_barQ = à partir du débit moyen 10 min  volets,
#' debit_vanne_barQ = à partir du débit moyen 10 min  vannes,
#' debit_barQ = volume_total_barQ / 86400.
#' @export
#'
#' @importFrom dplyr bind_cols select group_by summarize across starts_with
#' @importFrom dplyr arrange slice contains
#' 
#' 
#'  
debit_journalier <- function(debit_barrage, type = c("recalcule","barrage_volume","barrage_debit")){
  type <- match.arg(type)
  debit_barrage <- traitement_siva(dat=debit_barrage)
  Q12345 <- debit_total(param, param0 = param, debit_barrage)
  Q12345$tot_vol <- debit_barrage$tot_vol
  
  if (type == "recalcule") {
    Qj <-
      Q12345 %>%dplyr::select(
        date,
        Q,
        volvoletcalcule,
        debit_moyen_cran,
        tot_vol_siphon,
        tot_vol_passe,
        tot_vol_ecluse,
        tot_vol
      ) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(
        vol_recalcule = 
          sum(Q * 600, volvoletcalcule, tot_vol_passe, tot_vol_siphon,
              tot_vol_ecluse, na.rm =TRUE)
      ) %>%
      dplyr::mutate(
        debit_moyen_recalcule = vol_recalcule/(24 * 60 * 60)
      )
    
    Qj <- Qj %>% 
      dplyr::mutate(dplyr::across(dplyr::starts_with("vol"), round)) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("debit"),  ~ round(.x,digits=3))) %>%
      dplyr::arrange(date) %>%
      dplyr::slice(-1)
    return(Qj)
    
    
  } else if (type == "barrage_volume"){
    Q2j <- Q12345 %>%dplyr::select(
      date,
      tot_vol_vanne,
      tot_vol_volet,
      tot_vol_passe,
      tot_vol_siphon,
      tot_vol_ecluse,
      tot_vol
    ) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(
        vol_bar = sum(tot_vol, na.rm = TRUE),
        vol_vanne = sum(tot_vol_vanne, na.rm = TRUE),
        vol_volet = sum(tot_vol_volet, na.rm = TRUE),
        vol_passe = sum(tot_vol_passe, na.rm = TRUE),
        vol_siphon = sum(tot_vol_siphon, na.rm = TRUE),
        vol_ecluse = sum(tot_vol_ecluse, na.rm = TRUE)
      ) %>%
      dplyr::mutate(
        debit_moyen_vol_bar = vol_bar / (24 * 60 * 60),
        debit_moyen_vol_vanne = vol_vanne / (24 * 60 * 60),
        debit_moyen_vol_volet = vol_volet / (24 * 60 * 60),
        debit_moyen_vol_passe = vol_passe/ (24 * 60 * 60),
        debit_moyen_vol_siphon = vol_siphon/ (24 * 60 * 60),
        debit_moyen_vol_ecluse = vol_ecluse/ (24 * 60 * 60)
      )
    
    Q2j <- Q2j %>% 
      dplyr::mutate(dplyr::across(dplyr::starts_with("vol"), round)) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("debit"),  ~ round(.x,digits=3))) %>%
      dplyr::arrange(date) %>%
      dplyr::slice(-1)
    return(Q2j)
  } else if (type == "barrage_debit"){
    
    Qdj <- debit_barrage %>% dplyr::select (horodate, debit_vilaine_estime, debit_moyen_cran) %>%
      dplyr::mutate(date = as.Date(horodate)) %>%
      dplyr::select(-horodate) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(
        debit_vilaine_estime = sum(debit_vilaine_estime*600, na.rm = TRUE)/86400,
        debit_moyen_cran = sum(debit_moyen_cran*600, na.rm=TRUE)/86400) 
    
    date <-   debit_barrage %>%
      dplyr::select(horodate) %>%
      dplyr::mutate(date = as.Date(horodate))
    
    volets <- debit_barrage %>%
      dplyr::select(starts_with("debit_volet")) %>%
      dplyr::mutate(debit_volet_barQ = rowSums(., na.rm = TRUE),
                    volume_volet_barQ = debit_volet_barQ * 600)
    
    vannes <- debit_barrage %>%
      dplyr::select(starts_with("debit_vanne")) %>%
      dplyr::mutate(debit_vanne_barQ = rowSums(., na.rm = TRUE),
                    volume_vanne_barQ = debit_vanne_barQ * 600) 
    
    
    Q3j <- bind_cols(date, dplyr::bind_cols(volets, vannes)) %>%
      dplyr::select(date,
                    volume_volet_barQ,
                    volume_vanne_barQ) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(
        vol_vanne_barQ = sum(volume_vanne_barQ, na.rm = TRUE),
        vol_volet_barQ = sum(volume_volet_barQ, na.rm = TRUE)
      ) %>%
      dplyr::inner_join(
        debit_barrage %>% 
          dplyr::select(c("horodate","tot_vol_passe","tot_vol_siphon","tot_vol_ecluse")) %>%
          dplyr::mutate(date = as.Date(horodate)) %>%
          dplyr::select(-horodate) %>%
          dplyr::group_by(date) %>%
          dplyr::summarize(
            vol_passe = sum(tot_vol_passe, na.rm = TRUE),
            vol_siphon = sum(tot_vol_siphon, na.rm = TRUE),
            vol_ecluse = sum(tot_vol_ecluse, na.rm = TRUE)),
        by = "date") %>% 
      dplyr::inner_join(Qdj, by="date") %>% 
      dplyr::mutate(
        volume_total_barQ = dplyr::select(., dplyr::contains(c("vol_vanne_barQ", "vol_volet_barQ",  "vol_passe", "vol_siphon",
                                                        "vol_ecluse"))) %>% rowSums(na.rm =TRUE),
        debit_barQ = volume_total_barQ/ (24 * 60 * 60),
        debit_vanne_barQ = vol_vanne_barQ/ (24 * 60 * 60),
        debit_volet_barQ = vol_volet_barQ/ (24 * 60 * 60)
      ) %>% dplyr::select (-c("vol_passe","vol_siphon","vol_ecluse"))
    
    
    Q3j <- Q3j %>% 
      dplyr::mutate(across(starts_with("vol"), round)) %>%
      dplyr::mutate(across(starts_with("debit"),  ~ round(.x,digits=3))) %>%
      dplyr::arrange(date) %>%
      dplyr::slice(-1)
    return(Q3j)
    
  }
  
  
}