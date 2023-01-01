#' @title Plot avec le débit passe barrage et le débit recalculé
#' @description retourne un ggplot
#' @param ll Une liste avec un un data frame dat : le tableau des niveaux du df
#' @param ll la liste retournée par calc_fonct_passe
#' @export
plot_debit_passe <- function(ll) {
  bleu_EV <- "#00218f"
  turquoise_EV <- "#00C9C4"
  orange_EV <- "#ff7557"
  jaune_EV <- "#ffb428"
  bleu_EVf <- "#001350"
  jaune_EVf <- "#AD7000"
  orange_EVf <- "#b2513c"
  bleu_clair_EV <- "#33b5ff"
  turquoise_EVf <- "#007873"
  data_DFgr <- ll$dat %>%
    dplyr::select("horodate", starts_with("debit_passe")) %>%
    dplyr::rename(debit_passe_barrage=debit_passe) %>%
    tidyr::pivot_longer(cols=c(dplyr::starts_with("debit_passe_")),
                        names_to = "source",
                        values_to = "Q",
                        names_prefix = "debit_passe_")
  ggplot2::ggplot(data_DFgr) + 
    ggplot2::geom_point(ggplot2::aes(x=horodate, y=Q, color=source) , alpha=0.6) +
    ggplot2::geom_line(ggplot2::aes(x=horodate, y=Q, color=source), alpha=0.6 ) +
   
    ggplot2::geom_smooth(ggplot2::aes(x=horodate, y=Q, colour=source, fill=source)) +
    ggplot2::scale_fill_manual(values=c("barrage"=bleu_EVf,
                                        "recalcule"=orange_EVf))+
    ggplot2::scale_colour_manual(values=c("barrage"=bleu_EV,
                                          "recalcule"=orange_EV)) +
    ggplot2::theme_light()
}