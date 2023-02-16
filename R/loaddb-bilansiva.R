#' @title Methode de chargement pour loadb
#' @param objet Un objet de class bilansiva.
#' @param con Une connexion pool
#' @param checktag Defaut FALSE, faut il vérifier que le tag existe dans la table,
#' à utiliser lors des tests.
#' @param plot Boolean, faut il un graphe ?
#' @importFrom RMariaDB MariaDB
#' @importFrom pool dbPool
#' @importFrom lubridate round_date
#' @importFrom lubridate POSIXct
#' @return Un objet de classe bilansiva
#' @examples
#' \dontrun{
#' pool <- pool::dbPool(
#'    drv = RMariaDB::MariaDB(),
#'    dbname = "archive_IAV",
#'    host = hostmysql.,
#'    username = umysql.,
#'    password = pwdmysql.
#' )
#' bil<-new("bilansiva",
#'         tables=c("b_langon_debit", 
#'                  "b_pont_de_cran_debit"
#'         ),
#'         noms=c(
#'           "debit_langon",
#'           "debit_moyen_cran"          
#'         ),
#'         tags=as.integer(c(3000,1900)),
#'         daterondes=rep("constant",2), 
#'         debut=as.POSIXct(strptime(paste0(2019,"-05-01 00:00:00"),
#'                                   format="%Y-%m-%d %H:%M:%S")),
#'         fin=as.POSIXct(strptime(paste0(2019,"-09-01 00:00:00"),
#'                                 format="%Y-%m-%d %H:%M:%S"))
#' )
#' res <- loaddb(bil, con = pool)
#' poolClose(pool)
#' }
#' @export
setMethod(
  "loaddb",
  signature = signature("bilansiva"),
  definition = function(objet,
                        con,
                        checktag = FALSE,
                        plot = TRUE
                       ) {
    
      if (any(!is.integer(objet@tags))) stop("Les tags doivent \u00eatre des entiers, utiliser as.integer()")
      for (i in 1:length(objet@tables)) {
        tab <- new("tablesiva") # création de la classe
        tab@debut = objet@debut # attribut de la date de début de la classe prendre légèrement plus large que la période souhaitée
        tab@fin <-
          objet@fin  # attribut de la date de début de la classe
        tab@table <- objet@tables[i] # nom de la variable
        tab@nom <- objet@noms[i] # nom de la variable (dans R)
        tab@tag <-
          objet@tags[i] # soit NA soit le tag de la variable lorsque plusieurs variables sont dans la même table
        tab <-
          loaddb(tab , checktag = checktag, con = con) # load d'un data.frame dans le slot @rawdata (méthode de classe)
        if (nrow(tab@rawdata) > 0) {
          rounded10 <- lubridate::round_date(tab@rawdata$horodate , "10 mins")
          # le test est ignoré si les horodates sont déjà arrondies, car la 
          # conversion est alors inutile !
          if (any(tab@rawdata$horodate != rounded10)) {
            cat(sprintf("Des dates ne sont pas arrondies, application de la m\u00e9thode %s \n",objet@daterondes[i]))
            switch(
              objet@daterondes[i],
              constant = {
                # remplit le slot corrdata
                tab <- datesrondes(tab, method = "constant")
                print(
                  paste(
                    "methode d\'arrondi constant appliqu\u00e9e pour",
                    objet@noms[i],
                    "\n"
                  )
                )
              },
              linear = {
                # remplit le slot corrdata
                tab <- datesrondes(tab, method = "linear")
                print(
                  paste(
                    "methode d\'arrondi linear appliqu\u00e9e pour",
                    objet@noms[i],
                    "\n"
                  )
                )
              },
              none = {
                print(paste("pas d\'arrondi dans la date pour", objet@noms[i], "\n"))
              }
            )
          } # end test any rounded horodate
          if (plot)
            sivaplot(tab)
        } 
        # On utilise cette variable pour remplacer par la suite
        # test si on a corrigé les données
        if (nrow(tab@corrdata)>0){
          data_to_merge <-  tab@corrdata
        } else {
          data_to_merge <-  tab@rawdata
        }
        if (i == 1) {
          objet@bilandata <- data_to_merge
    
        } else {
          objet@bilandata <-
            merge(
              objet@bilandata,
              data_to_merge,
              by = "horodate",
              all.x = TRUE,
              all.y = TRUE
            )
        }
        #poolClose(pool)
        
      }
        cat("fin des calculs \n")

    
    return(objet)
  }
)