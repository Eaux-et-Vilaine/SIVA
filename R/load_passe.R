#' Chargement des donnees de debit de la passe
#' 
#' Il faut avoir configuré la connexion à la base
#'
#' @param debut La date de début format POSIXct ou character "%Y-%m-%d %H:%M:%S
#' @param fin La fin format POSIXct ou character "%Y-%m-%d %H:%M:%S
#' @param con une connexion pool
#' @return Le tableau de données pour le calcul du fonctionnement de la passe
#' @export
#' @importFrom dplyr tribble
#'
#' @examples
#' \dontrun{
#' load_passe(debut = 
#' as.POSIXct(strptime("2010-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")),
#'    fin = 
#'    as.POSIXct(strptime("2010-01-10 00:00:00", format = "%Y-%m-%d %H:%M:%S")),
#'    con= con)
#'    }
load_passe <- function(debut, fin, con){
  if (!is.POSIXct(debut)) debut <- as.POSIXct(strptime(debut, format = "%Y-%m-%d %H:%M:%S"))
  if (!is.POSIXct(fin)) fin <-as.POSIXct(strptime(fin, format = "%Y-%m-%d %H:%M:%S"))
  Sys.setenv(TZ='GMT')		
  df<- dplyr::tribble(
      ~noms, ~tables, ~tags,
      "niveau_mer_passe","b_passeapoisson_niveaumer",   NA,
      "niveau_sas_passe","b_passeapoisson_niveausas",   NA,
      "niveau_vilaine_passe","b_passeapoisson_niveauvilaine", NA,
      "volume_cumule_passe","b_barrage_debit",   2523,
      "niveau_mer_barrage","b_barrage_niveau", 2508,
      "niveau_vilaine_barrage","b_barrage_niveau", 2507,
      "hauteur_vanne_mer","b_passeapoisson_hauteurvannemer", NA,
      "hauteur_vanne_vilaine","b_passeapoisson_hauteurvannevilaine", NA,
      "debit_passe","b_barrage_debit", 2523,       # volume jour passe
      #"temperature_corde1_cran","b_pont_de_cran_vitesse",1909,
      #"temperature_corde2_cran","b_pont_de_cran_vitesse",1910,		
      #"temperature_externe_barrage","b_barrage_temperature",2560,	#temperature exterieure
      #"debit_pont_de_cran","b_pont_de_cran_debit",1900,
      #"debit_vilaine_estime","b_barrage_debit", 2515, 
  )
  bil<-new("bilansiva",
      tables=df$tables,
      noms=df$noms,
      tags=as.integer(df$tags),
      daterondes=rep("constant",nrow(df)),
      debut=debut,
      fin=fin
  )
  dat <- loaddb(bil,plot=FALSE, con=con)@bilandata
  return(dat)
}