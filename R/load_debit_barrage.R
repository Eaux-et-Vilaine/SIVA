#' Chargement des donnees de debit du barrage
#' 
#' Il faut avoir configuré la connexion à la base
#'
#' @param debut La date de début format POSIXct ou character "%Y-%m-%d %H:%M:%S
#' @param fin La fin format POSIXct ou character "%Y-%m-%d %H:%M:%S
#' @param con une connexion pool
#' @return Le tableau de données pour le calcul des debits du barrage
#' @export
#'
#' @examples
#' \dontrun{
#' load_debit_barrage(debut = 
#' as.POSIXct(strptime("2010-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")),
#'    fin = 
#'    as.POSIXct(strptime("2010-01-10 00:00:00", format = "%Y-%m-%d %H:%M:%S")),
#'    con= con)
#'    }
load_debit_barrage <- function(debut, fin, con){
  if (!is.POSIXct(debut)) debut <- as.POSIXct(strptime(debut, format = "%Y-%m-%d %H:%M:%S"))
  if (!is.POSIXct(fin)) fin <-as.POSIXct(strptime(fin, format = "%Y-%m-%d %H:%M:%S"))
  Sys.setenv(TZ='GMT')		
  bil<-new("bilansiva",
           tables=c("b_barrage_volet1_hauteur",
                    "b_barrage_volet2_hauteur",
                    "b_barrage_volet3_hauteur",
                    "b_barrage_volet4_hauteur",
                    "b_barrage_volet5_hauteur",
                    "b_barrage_vanne1_hauteur",
                    "b_barrage_vanne2_hauteur",
                    "b_barrage_vanne3_hauteur",
                    "b_barrage_vanne4_hauteur",
                    "b_barrage_vanne5_hauteur",
                    "b_barrage_debit", # Débit Vilaine estimé
                    "b_barrage_debit", #débit passe
                    "b_pont_de_cran_debit",
                    rep("b_barrage_volume",5),
                    "b_passeapoisson_niveauvilaine",
                    "b_passeapoisson_niveaumer",
                    rep("b_barrage_niveau",2),
                    rep("b_siphon_debit",2),
                    rep("b_barrage_debit",10)
           ),
           noms=c("volet1",
                  "volet2",
                  "volet3",
                  "volet4",
                  "volet5",
                  "vanne1",
                  "vanne2",
                  "vanne3",
                  "vanne4",
                  "vanne5",
                  "debit_vilaine_estime",
                  "debit_passe",
                  "debit_moyen_cran",
                  "tot_vol_barrage",
                  "tot_vol_passe",
                  "tot_vol_siphon",
                  "tot_vol_volet",
                  "tot_vol_ecluse",
                  "niveauvilaine",
                  "niveaumer",
                  "niveauvilaineb",
                  "niveaumerb",
                  "debit_siphon_1",
                  "debit_siphon_2",
                  "debit_vanne1",
                  "debit_vanne2",
                  "debit_vanne3",
                  "debit_vanne4",
                  "debit_vanne5",
                  "debit_volet1",
                  "debit_volet2",
                  "debit_volet3",
                  "debit_volet4",
                  "debit_volet5"
           ),
           tags=c(as.integer(c(2555:2559,
                               2509:2513,
                                2515,2523,
                               1900, # pont de cran
                               2550:2554, # tot_vol
                               2519,2520,2507,2508, # niveaux barrage
                               1528, #siphon debit
                               1565,  #siphon debit 2
                               2571, #debit vanne ....
                               2572, 
                               2573,
                               2574,
                               2575,
                               2581,
                               2582,
                               2583,
                               2584,
                               2585
                               
                  ))),
           daterondes=c(rep("constant",10),rep("linear",24)),
           debut=debut,
           fin=fin
           )
  dat <- loaddb(bil,plot=FALSE, con=con)@bilandata
  return(dat)
}