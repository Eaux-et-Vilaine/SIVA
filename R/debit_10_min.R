



#' Débit du barrage.
#' 
#' Attention la valeur du jour 1 est NA à cause des totaliseurs.
#'
#' @param debit_barrage Le jeu de données issu du chargement `load_debit_barrage`.
#'
#' @return Un tableau de données de débit journier.
#' Un tibble avec : 
#' debit_barQ Le débit source barrage à partir des volumes écluse, passe,
#'  siphon et #' des débits mesurés sur les vannes et les volets
#' debit_recalculé Le débit recalculé y compris à partir des volumes écluse,
#'  passe, siphon

#' @export
#'
#' @importFrom dplyr bind_cols select group_by summarize across starts_with
#' @importFrom dplyr arrange slice contains
#' 
#' 
#'  
debit_10_min <- function(debit_barrage){

  debit_barrage <- traitement_siva(debit_barrage)
  Q12345 <- debit_total(param, param0 = param, debit_barrage)

  
  
    Q10_1 <-
      Q12345 %>%dplyr::select(
        horodate,
        Q,
        volvoletcalcule,
        tot_vol_siphon,
        tot_vol_passe,
        tot_vol_ecluse
      ) %>%
      dplyr::mutate(
        volvannescalcule= Q *600) %>%
      dplyr::mutate(
        vol_recalcule = 
          rowSums(select(., volvannescalcule , volvoletcalcule, tot_vol_passe, tot_vol_siphon,
              tot_vol_ecluse), na.rm =TRUE)
      ) %>%
      dplyr::mutate(
        debit_recalcule = vol_recalcule/600
      ) %>%
      select(horodate,debit_recalcule)
   
    volets <- debit_barrage %>%
      dplyr::select(starts_with("debit_volet")) %>%
      dplyr::mutate(debit_volet_barQ = rowSums(., na.rm = TRUE),
                    volume_volet_barQ = debit_volet_barQ * 600)
    
    vannes <- debit_barrage %>%
      dplyr::select(starts_with("debit_vanne")) %>%
      dplyr::mutate(debit_vanne_barQ = rowSums(., na.rm = TRUE),
                    volume_vanne_barQ = debit_vanne_barQ * 600) 
    
    
    Q10_2 <- dplyr::bind_cols( dplyr::bind_cols(volets, vannes)) %>%
      dplyr::select(
                    volume_volet_barQ,
                    volume_vanne_barQ) %>%
      dplyr::bind_cols(
        debit_barrage %>% 
          dplyr::select(c("tot_vol_passe","tot_vol_siphon","tot_vol_ecluse"))) %>%
      dplyr::mutate(
        volume_total_barQ = dplyr::select(., dplyr::contains(c("volume_vanne_barQ", "volume_volet_barQ",  "tot_vol_passe", "tot_vol_siphon",
                                                               "tot_vol_ecluse"))) %>% rowSums(na.rm =TRUE),
        debit_barQ = volume_total_barQ/ 600
      ) %>% dplyr::select (debit_barQ)
  
    
    Q10 <-bind_cols(Q10_1,Q10_2) %>%
      dplyr::mutate(across(starts_with("debit"),  ~ round(.x,digits=3))) %>%
      dplyr::arrange(horodate)
    return(Q10)
  
}