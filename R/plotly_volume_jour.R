#' graphique des volumes journaliers
#'
#' @param QV Un dataset des volumes journaliers (voir vignette plot) 
#'
#' @return Un graphique plotly
#' @importFrom tidyr pivot_longer
#' @importFrom plotly ggplotly
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 theme_light 
#' @importFrom ggplot2 ylab
#' @export
#'
#' @examples
#' \dontrun{
#' plotly_volume_jour(QV=QV)
#' }
plotly_volume_jour <- function(QV){
  bleu_EV <- "#00218f"
  turquoise_EV <- "#00C9C4"
  orange_EV <- "#ff7557"
  jaune_EV <- "#ffb428"
  bleu_EVf <- "#001350"
  jaune_EVf <- "#AD7000"
  orange_EVf <- "#b2513c"
  bleu_clair_EV <- "#33b5ff"
  turquoise_EVf <- "#007873"
  
  V <-  QV %>% dplyr::select(date |  dplyr::starts_with("vol")) %>%
      dplyr::select(-c(vol_bar,vol_volet,vol_vanne)) %>%  # mauvaises valeurs
      dplyr::select(-c(vol_recalcule,volume_total_barQ)) %>%
      tidyr::pivot_longer(
          cols = dplyr::starts_with("vol"),
          names_to = "source",
          names_prefix = "vol_",
          values_to = "vol"
      ) %>%
      dplyr::mutate(vol=dplyr::case_when(vol/10^6 < 0.01 ~ round(vol/10^6,3),
              vol/10^6 < 10 ~ round(vol/10^6,1),
              TRUE ~ round(vol/10^6)),
          source=gsub("_barQ","",.$source))
  Q <- QV %>% dplyr::select(date |  dplyr::starts_with("debit")) %>%
      dplyr::select(date,
          debit_moyen_vol_passe, 
          debit_moyen_vol_siphon, 
          debit_moyen_vol_ecluse,
          debit_vanne_barQ, 
          debit_volet_barQ) %>%
      tidyr::pivot_longer(
          cols = dplyr::starts_with("debit"),
          names_to = "source",
          names_prefix = "debit_",
          values_to = "Q"
      ) %>% 
      dplyr::mutate(
          source=gsub("_barQ","",.$source))%>% 
      dplyr::mutate(
          source=gsub("moyen_vol_","",.$source),
          Q = round(Q)) 
  
  QV2 <- dplyr::inner_join(V,Q, by = c("date","source"))
  QV2$source <- factor(QV2$source,levels=c("ecluse","passe","siphon","volet","vanne")) 
  QV2$Qtxt = paste("Q",substr(QV2$source,1,2),"=\n",QV2$Q,"m3/s")
  QV2$Qtxt[Q$Q==0] <- ""
  # QV2 <- inner_join(QV2,data.frame(
  #   source=c("vanne","volet","ecluse","passe","siphon"), 
  #   color=c(bleu_EVf,turquoise_EV,jaune_EV,orange_EV,bleu_clair_EV)))
  
  
  # Essai avec ggplotly, on peut pas ajuster le hover
  g <- ggplot2::ggplot(QV2) + ggplot2::geom_col(ggplot2::aes(x=date, y= vol, fill = source, customdata = date), position="stack") +
      #ylab(expression(paste(Volume~10^6~m^3))) +
      ggplot2::ylab("Volume en millions de m3") +
      ggplot2::scale_fill_manual(values=c("vanne"=bleu_EVf,
              "volet"=turquoise_EV,
              "ecluse"=jaune_EV,
              "passe"=orange_EV,
              "siphon"= bleu_clair_EV)) +
      ggplot2::geom_text(ggplot2::aes(x=date,y=vol,label=Qtxt),size=1.8,col="turquoise")+
      ggplot2::theme_light()
  y <- plotly::ggplotly(g, source = "volume_jour")
  return(y)
}