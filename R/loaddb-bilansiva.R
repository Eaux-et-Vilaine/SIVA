#' @title Methode de chargement pour loadb
#' @param objet Un objet de class bilansiva.
#' @param datawd Le chemin vers le repertoire où sauver les données.
#' @param con A pool connexion object
#' @param plot Boolean, faut il un graphe ?
#' @param loadrdata Faut il charger un jeu de donnees sauvé dans datawd à la place de la base
#' @param saverdata Faut il sauvegarder un jeu de donnees dans datawd 
#' @importFrom RMariaDB MariaDB
#' @importFrom pool dbPool
#' @return Un objet de classe bilansiva
#' @examples 
#' \dontrun{
#' pool <- pool::dbPool(
#'    drv = RMariaDB::MariaDB(),
#'    dbname = "archive_IAV",
#'    host = hostmysql.,
#'    username = umysql.,
#'    password = pwdmysql.
#')

#'tablesiva <-
#'   new(
#'     "tablesiva",
#'     debut = as.POSIXct(as.Date("2021-10-25")),
#'     fin =  as.POSIXct(as.Date("2021-12-01")),
#'    table = "b_barrage_volet4_hauteur",
#'     nom = "volet4"
#'  )
#' loaddb(tablesiva, con=pool)}
#' @export
setMethod(
    "loaddb",
    signature = signature("bilansiva"),
    definition = function(objet,
        datawd,
        con,
        plot = TRUE,
        loadrdata = FALSE,
        saverdata = FALSE) {
      if (loadrdata) {
        # charge un objet precedemment sauve
        datapath <- file.path(
            datawd,
            paste0(
                "bilansiva",
                strftime(objet@debut, "%d%m%y"),
                "-",
                strftime(objet@fin, "%d%m%y"),
                ".Rdata"
            ))
        if (! file.exists(datapath)) stop("L'objet a charger n'existe pas, lancer d'abord un chargement depuis la base 
               avec loaddb(objet = monbilansiva, con=maconnexionpool, loadrdata=FALSE, saverdata=TRUE")
        load(file = datapath )
      } else {
        
        for (i in 1:length(objet@tables)) {
          tab <- new("tablesiva") # création de la classe
          tab@debut = objet@debut # attribut de la date de début de la classe prendre légèrement plus large que la période souhaitée
          tab@fin <-
              objet@fin  # attribut de la date de début de la classe
          tab@table <- objet@tables[i] # nom de la variable
          tab@nom <- objet@noms[i] # nom de la variable (dans R)
          tab@tag <-
              objet@tags[i] # soit NA soit le tag de la variable lorsque plusieurs variables sont dans la même table
          tab <- loaddb(tab , con=con) # load d'un data.frame dans le slot @rawdata (méthode de classe)
          if (nrow(tab@rawdata) > 0) {
            switch(
                objet@daterondes[i],
                constant = {
                  tab <- datesrondes(tab, method = "constant")
                  print(paste(
                          "methode d\'arrondi constant appliqu\u00e9e pour",
                          objet@noms[i],
                          "\n"
                      ))
                },
                linear = {
                  tab <- datesrondes(tab, method = "linear")
                  print(paste(
                          "methode d\'arrondi linear appliqu\u00e9e pour",
                          objet@noms[i],
                          "\n"
                      ))
                },
                none = {
                  print(paste("pas d\'arrondi dans la date pour", objet@noms[i], "\n"))
                  tab@corrdata <- tab@rawdata[, c(2, 3)] # j'enlève le tag
                }
            )
            if (plot)
              sivaplot(tab)
          } else {
            tab@corrdata <- tab@rawdata
          }
          if (i == 1) {
            objet@bilandata <- tab@corrdata
          } else {
            objet@bilandata <-
                merge(
                    objet@bilandata,
                    tab@corrdata,
                    by = "HoroDate",
                    all.x = TRUE,
                    all.y = TRUE
                )
          }
          #poolClose(pool)
          
        }
      }
      cat("fin des calculs \n")
      # sauvegarde de l'objet ------------------
      if(saverdata){
        save(objet,file.path(
                datawd,
                paste0(
                    "bilansiva",
                    strftime(objet@debut, "%d%m%y"),
                    "-",
                    strftime(objet@fin, "%d%m%y"),
                    ".Rdata"
                )))
      }
      return(objet)
    }
)