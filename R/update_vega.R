#' Mets à jour les dernières données depuis l'interface REST de la sonde VEGA
#' 
#' Récupère la date de la dernière donnée puis mets à jour la table correspondant
#' à Trevelo ou Isac
#' @param con Une connexion pool vers SIVA
#' @param sonde "Trevelo" ou "Isac"
#' @param apiKey La clé pour se connecter à l'interface rest de Vega
#'
#' @return NULL
#' @export
#' @importFrom DBI dbExecute
#' @importFrom DBI dbGetQuery
#' @importFrom DBI dbWriteTable
#'
#' @examples
#' \dontrun{
#' update_vega(con = pool, sonde = "Trevelo", apiKey=apikey)
#' update_vega(con= pool, sonde = "Isac", apiKey=apikey)
#' }
update_vega <- function(con, 
    sonde="Trevelo", 
    apiKey){
  if (class(con)[[1]]!="Pool") stop ("con doit \u00eatre une connexion")
  if (sonde == 'Isac'){
    debut <- DBI::dbGetQuery(con,"SELECT max(Horodate) hh FROM b_isac_niveaumarais;")
    temptable <- "tempisac"
    nomtable <- "b_isac_niveaumarais"
    tag=as.integer(384)    
  } else if (sonde == 'Trevelo'){
    debut <- DBI::dbGetQuery(con,"SELECT max(Horodate) hh FROM b_trevelo_niveaumarais;")
    temptable <- "temptrevelo"
    nomtable <- "b_trevelo_niveaumarais"
    tag = as.integer(430)
  } else {
    stop("sonde doit \u00eatre soit Isac soit Trevelo")
  }
  debut <- strftime(debut$hh, format = "%Y-%m-%d %H:%M", tz= "GMT")
  dat_vega <- load_vega (sonde=sonde, 
          horodate_debut = debut,
          apiKey=apiKey) %>%
      dplyr::mutate(Tag=tag) %>%
      dplyr::rename("HoroDate"="timestamp","Valeur"="value") %>%
      dplyr::select(Tag,HoroDate,Valeur)
  dat_vega <- dat_vega[-1,] # la dernière valeur est déjà dans la table
  if (nrow(dat_vega)>0){
  DBI::dbExecute(con,paste0("DROP TABLE IF EXISTS ",temptable,";"))
  DBI::dbWriteTable(con, temptable, dat_vega, row.names = FALSE, append = TRUE)
  DBI::dbExecute(con, paste0("INSERT INTO ",nomtable," SELECT * FROM ",temptable,";"))
  DBI::dbExecute(con,paste0("DROP TABLE IF EXISTS ",temptable,";"))
  cat(sprintf("%s Valeurs mise(s) \u00e0 jour pour %s depuis %s \u00e0 %s, table %s, tag %s",
          nrow(dat_vega),
          sonde, 
          min(dat_vega$HoroDate), 
          max(dat_vega$HoroDate),
          nomtable,
          tag))
} else {
  cat(sprintf("%s derni\u00e8re donn\u00e9e dans la table %s pas de nouvelle donn\u00e9es de la sonde",
          sonde,
          debut))
}
  return(invisible(NULL))
}

