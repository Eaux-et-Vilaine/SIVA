#' Calcul du débit sur tous les volets
#' 
#' Somme du débit de tous les volets
#' 
#' @param hvolet1 La hauteur du volet1.
#' @param hvolet2 La hauteur du volet2.
#' @param hvolet3 La hauteur du volet3.
#' @param hvolet4 La hauteur du volet4.
#' @param hvolet5 La hauteur du volet5.
#' @param hvilaine Le niveau Vilaine.
#' @param hvanne1 La hauteur de vanne.
#' @param hvanne2 La hauteur de vanne.
#' @param hvanne3 La hauteur de vanne. 
#' @param hvanne4 La hauteur de vanne. 
#' @param hvanne5 La hauteur de vanne. 
#' @param hmer Le niveau mer.
#' @param Cvo Le coefficient de calcul de débit sur les volets, défaut 0.627.
#'
#' @return Un data frame avec les colonnes Qvolet1 ... Qvolet5.
#' 
#' @export
debit_volets <-
  function(hvolet1,
           hvolet2,
           hvolet3,
           hvolet4,
           hvolet5,
           hvilaine,
           hvanne1,
           hvanne2,
           hvanne3,
           hvanne4,
           hvanne5,
           hmer,
           Cvo = 0.627) {
    Qv1 <- debit_volet(hvolet1, hvilaine, hvanne1, hmer, Cvo)
    Qv2 <- debit_volet(hvolet2, hvilaine, hvanne2, hmer, Cvo)
    Qv3 <- debit_volet(hvolet3, hvilaine, hvanne3, hmer, Cvo)
    Qv4 <- debit_volet(hvolet4, hvilaine, hvanne4, hmer, Cvo)
    Qv5 <- debit_volet(hvolet5, hvilaine, hvanne5, hmer, Cvo)
    return(
      data.frame(
        "Qvolet1" = Qv1,
        "Qvolet2" = Qv2,
        "Qvolet3" = Qv3,
        "Qvolet4" = Qv4,
        "Qvolet5" = Qv5
      )
    )
  }
