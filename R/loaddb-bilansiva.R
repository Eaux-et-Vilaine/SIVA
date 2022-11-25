#' @title Methode de chargement pour loadb
#' @param objet Un objet de class bilansiva.
#' @param datawd Le chemin vers le repertoire où sauver les données.
#' @param plot Boolean, faut il un graphe ?
#' @param restart Default NULL, si non null, l'objet est sauvé sous "tempsauvobjet" dans datawd.
#' @return Un objet de classe bilansiva
#' @export

setMethod(
  "loaddb",
  signature = signature("bilansiva"),
  definition = function(objet,
                        datawd,
                        plot = TRUE,
                        restart = NULL) {
    if (!is.null(restart)) {
      start <- restart
      load(file = file.path(
        datawd,
        paste0(
          "bilansiva",
          strftime(objet@debut, "%d%m%y"),
          "-",
          strftime(objet@fin, "%d%m%y"),
          ".Rdata"
        )
      )) #replace objet
    } else
      start = 1
    
    for (i in start:length(objet@tables)) {
      tab <- new("tablesiva") # création de la classe
      tab@debut = objet@debut # attribut de la date de début de la classe prendre légèrement plus large que la période souhaitée
      tab@fin <-
        objet@fin  # attribut de la date de début de la classe
      tab@table <- objet@tables[i] # nom de la variable
      tab@nom <- objet@noms[i] # nom de la variable (dans R)
      tab@tag <-
        objet@tags[i] # soit NA soit le tag de la variable lorsque plusieurs variables sont dans la même table
      tab <-
        loaddb(tab) # load d'un data.frame dans le slot @rawdata (méthode de classe)
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
      
      
    }
    cat("fin des calculs \n")
    # sauvegarde de l'objet ------------------
    save(objet,file.path(
      datawd,
      paste0(
        "bilansiva",
        strftime(objet@debut, "%d%m%y"),
        "-",
        strftime(objet@fin, "%d%m%y"),
        ".Rdata"
      )))
    return(objet)
  }
)