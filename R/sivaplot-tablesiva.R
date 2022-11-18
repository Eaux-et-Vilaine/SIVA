
#' Methode sivaplot
#' 
#' La méthode sivaplot permet d'aller chercher les données après avoir installé le driver odbc et avoir appellé la bonne chaine de connection (il doit y avoir une variable nommée sivacon dans le workspace)
#' @param objet, un objet de classe Tablesiva
#' @importFrom stringr str_c
#' @return Un object de classe Tablesiva
#' @exportMethod
setMethod(
    "sivaplot",
    signature = signature("Tablesiva"),
    definition = function(objet) {
      png(filename = str_c(imgwd, objet@nom, "_", label, ".png"))
      plot(
          objet@rawdata$HoroDate,
          objet@rawdata[, 3],
          type = "b",
          xlab = "date",
          ylab = "",
          main = objet@nom
      )
      points(objet@corrdata$HoroDate,
          objet@corrdata[, 2],
          col = "red",
          pch = "*")
      dev.off()
    }
)