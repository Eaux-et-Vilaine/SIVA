#' Collecte les valeurs de l'API vega
#'
#' @param sonde Soit "Trevelo" soit "Isac"
#' @param horodate_debut La date de début au format "2023-01-01 00:00"
#' @param horodate_fin La date de début au format "2023-01-01 00:00", défaut NULL,
#' correspond alors à la date du jour
#' @param apiKey La clé de l'API
#'
#' @return Un tableau de données avec "timestamp","value","state","isvalid" 
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom httr warn_for_status
#' @importFrom httr stop_for_status
#' @importFrom httr add_headers
#' @importFrom httr content
#'
#' @examples
#'  \dontrun{
#'  load_vega("Trevelo")
#'  load_vega("Isac")
#'  load_vega("Isac", horodate_debut="2023-01-01 00:00")
#'  }
load_vega <- function(sonde="Trevelo", 
    horodate_debut="2023-01-01 00:00",
    horodate_fin=NULL,
    apiKey){
  #browser()
  id <- switch(sonde, "Trevelo"="21642", "Isac"="18527")
  valuesapi <- "https://vis.vega.com/api/Values"  
  datedebut <- strptime(horodate_debut, format = "%Y-%m-%d %H:%M")
  datedebut <- strftime(datedebut, format ="%d%m%Y%H%M")
  if (is.null(horodate_fin)) {
    datefin <- Sys.Date()
    datefin <- strftime(datefin, format ="%d%m%Y%H%M")
  } else {
    datefin <- strptime(horodate_fin, format = "%Y-%m-%d %H:%M")
    datefin <- strftime(datefin, format ="%d%m%Y%H%M")
  }
  date <- paste(datedebut,datefin,sep="-")
  unit <- 2
  url <- paste(valuesapi, id, date, unit, sep="/")
  headers <- c("accept" = "text/json", 
      "X-Api-Key" = apiKey)
  res <- httr::GET(url,
      httr::add_headers(.headers=headers))
  httr::warn_for_status(res)
  httr::stop_for_status(res)
  content <- jsonlite::fromJSON(httr::content(res, as = "text"),simplifyVector = FALSE)
  df <- do.call(rbind.data.frame, content[[1]]$MeasuringConfigs[[1]]$Values)
  colnames(df) <- tolower(colnames(df))
  df$timestamp <- strptime(gsub("T"," ",df$timestamp), format = "%Y-%m-%d %H:%M:%S")
  return(df)
}