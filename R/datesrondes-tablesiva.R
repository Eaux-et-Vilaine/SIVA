
#' Generic function for datesrondes
#' @keywords internal
setGeneric("datesrondes",def=function(objet,...) standardGeneric("datesrondes"))

#' Methode daterondes
#' 
#' La méthode datesrondes permet d'arrondir les dates, les données sont passées de rawdata à corrdata. 
#' Deux options methode="constant" et method="linear" voir approx()
#' @param objet Un objet de classe tablesiva
#' @param method, méthode d'interpolation linéaire passée à approx, "linear" ou "constant" ou "none".
#' @export
#' @importFrom stats approx
setMethod(
  "datesrondes",
  signature = signature("tablesiva"),
  definition = function(objet, method = "constant") {
    raw <- objet@rawdata
    s <-
      seq.POSIXt(
        from = round(min(raw$horodate), "hour"),
        to = round(max(raw$horodate), "hour") + 1,
        by = "10 mins"
      )
    s <- s[s >= min(raw$horodate) & s <= max(raw$horodate)]
    s <- data.frame("horodate" = s)
    s$Valeur <-
      approx(
        raw$horodate,
        raw[, 2],
        xout = s$horodate,
        method = method,
        ties = "ordered"
      )$y
    objet@corrdata <- s
    colnames(objet@corrdata)[2] <- objet@nom
    return(objet)
  }
)



