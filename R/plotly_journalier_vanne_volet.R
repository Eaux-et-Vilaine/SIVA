#' Graphique du débit journalier vannes volets
#'
#' @param date 
#' @param debit_traite Un data frame avec les débits traités par`debit total`
#'
#' @return Un graphique ggplotly du débit journalier au barrage d'Arzal
#' @export
#'
#' @examples plotly_journalier_vanne_volet(date = "2018-01-01", debit_traite=Q12345)
plotly_journalier_vanne_volet <- function(date, debit_traite){
  if (is.character(date)) date <- as.Date(date)
  if (! date %in% debit_traite$date) stop(sprintf("Date %s n'est pas dans la période %s ", 
                                                  date,paste(range(debit_traite$date),collapse=" : ")))
  Qd <- debit_traite[debit_traite$date==date,c("horodate","qvanne1","qvanne2","qvanne3","qvanne4","qvanne5",
                                               "typecalc1","typecalc2","typecalc3","typecalc4","typecalc5",
                                               "qvolet1","qvolet2","qvolet3","qvolet4","qvolet5")] 
  
  
  Qd <-  Qd %>% 
    select("horodate", starts_with("qvanne"),starts_with("qvolet")) %>%
    pivot_longer(cols=c(starts_with("qvanne"),starts_with("qvolet")),
                 names_to = "fonctionnement",
                 values_to = "Q",
                 names_prefix = "q")
  Qd$Q <- round(Qd$Q)
  Qd$horaire <- strftime(Qd$horodate, format = "%H:%M:%S")
  Qd$fonctionnement <- factor(Qd$fonctionnement, levels = c("vanne1","volet1", 
                                                            "vanne2", "volet2",
                                                            "vanne3", "volet3",
                                                            "vanne4", "volet4",
                                                            "vanne5", "volet5"))
  g <- ggplot(Qd) + geom_tile(aes(x=fonctionnement,y=horodate,fill=Q)) + viridis::scale_fill_viridis() +
    theme_minimal()
  return(plotly::ggplotly(g))
  
}