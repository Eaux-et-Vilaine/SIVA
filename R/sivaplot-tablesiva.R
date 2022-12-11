
#' Generic function for plot
#' @keywords internal
setGeneric("sivaplot",def=function(objet,...) standardGeneric("sivaplot"))
#' Methode sivaplot
#' 
#' La méthode sivaplot permet d'aller chercher les données après avoir installé le driver odbc et avoir appellé la bonne chaine de connection (il doit y avoir une variable nommée sivacon dans le workspace)
#' @param objet, un objet de classe tablesiva
#' @importFrom stringr str_c
#' @return Un object de classe tablesiva
#' @importFrom graphics points
#' @export
setMethod(
    "sivaplot",
    signature = signature("tablesiva"),
    definition = function(objet) {
      plot(
          objet@rawdata$horodate,
          objet@rawdata[, 2],
          type = "b",
          xlab = "date",
          ylab = "",
          main = objet@nom
      )
      if (nrow(objet@corrdata >0)) {
      graphics::points(objet@corrdata$horodate,
          objet@corrdata[, 2],
          col = "red",
          pch = "*")
      }
    }
)