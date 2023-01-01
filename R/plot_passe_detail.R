#' @title Plot avec lignes et indications du fonctionnement du DF
#' @description retourne un ggplot
#' @param data_DF le tableau des niveaux du df
#' avec une date de début une date de fin et un changement à chaque étape.
#' @param debut date de début, character or date
#' @param fin date de fin
#' @return Un graphique ggplot
#' @importFrom lubridate parse_date_time
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #plot_passe_detail(ll$dat)
#'  }
#' }
#' @export
plot_passe_detail <- function(data_DF, debut, fin) {
  bleu_EV <- "#00218f"
  turquoise_EV <- "#00C9C4"
  orange_EV <- "#ff7557"
  jaune_EV <- "#ffb428"
  bleu_EVf <- "#001350"
  jaune_EVf <- "#AD7000"
  orange_EVf <- "#b2513c"
  bleu_clair_EV <- "#33b5ff"
  turquoise_EVf <- "#007873"
  
  if (missing(debut)) {
    debut <- data_DF$horodate[1]
    cat("Manque le d\u00e9but, remplacement par la date de d\u00e9but \n")
  }
  if (typeof(debut) == "character") {
    debut <- lubridate::parse_date_time(debut, orders = c("dmy", "ymd"))
    cat("debut \u00e9valu\u00e9 comme ",
        strftime(debut, "%d-%m-%Y "), "\n")
  }
  if (missing(fin)) {
    fin <- debut + as.difftime(1, units = "weeks")
    cat("fin manquant, defaut 1 semaine", "\n")
  }
  if (typeof(fin) == "character") {
    fin <- lubridate::parse_date_time(fin, orders = c("dmy", "ymd"))
    cat("fin \u00e9valu\u00e9 comme ", strftime(fin, "%d-%m-%Y"),"\n")
  }
  data_DFw <- data_DF %>% dplyr::filter(horodate < fin & horodate > debut)
  
  data_DFw$per_tar_code <- as.factor(data_DFw$per_tar_code)
  levels(data_DFw$per_tar_code) <- 
      c(
      "Fonc normal",
      "Arr ponctuel",
      "Arr maint",
      "Dysfonc",
      "Non connu"
  )
  
  g <- ggplot2::ggplot(data_DFw) +
      ggplot2::geom_line(ggplot2::aes(x = horodate, y = niveau_mer_barrage),
          col = orange_EV,
          lty = 1) +
      ggplot2::geom_line(ggplot2::aes(x = horodate, y = niveau_vilaine_barrage),
          col = bleu_EV,
          lty = 4) +
      ggplot2::geom_line(ggplot2::aes(x = horodate, y = niveau_sas_passe), col = "mediumslateblue") +
      ggplot2::geom_line(ggplot2::aes(x = horodate, y = hauteur_vanne_mer),
          col = "purple",
          lty = 3) +
      ggplot2::geom_line(
          ggplot2::aes(x = horodate, y = hauteur_vanne_vilaine),
          col = "darkgoldenrod",
          lty = 1
      ) +
      ggplot2::geom_rect(
          ggplot2::aes(
              xmin = horodate,
              xmax = horodate + as.difftime(10, units = "mins"),
              ymin = -3.5,
              ymax = -3,
              fill = per_tar_code
          ),
          alpha = 0.8
      ) +
      ggplot2::theme_bw() +
      ggplot2::scale_fill_manual(
          values = c("#17364E", "moccasin", "#765223", "#B68B52", "grey"),
          name = "type d\'arr\u00eat",          
          breaks = c(
              "Fonc normal",
              "Arr ponctuel",
              "Arr maint",
              "Dysfonc",
              "Non connu"
          )
      ) +
      ylab("Niveau en m")
  
  
  return(g)
  
}