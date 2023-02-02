#' Variables pour l'ISAC et pluviometrie dans SIVA
#'
#' extrait depuis SIVA_R voir data_raw variable.R
#'
#' @format ## `variable`
#' Un data frame avec 17 lignes et 8 colonnes:
#' \describe{
#'   \item{code}{Code interne pour appel de la variable.}
#'   \item{tag}{Le tag.}  
#'   \item{tagstation}{Le tag de la station.}
#'   \item{libelle}{Le libelle modifié depuis SIVA.}
#'   \item{unite}{L'unite.}
#'   \item{tablehisto}{Le nom de la table dans la base.}
#'   \item{isac}{Les variables appelées par défaut dans le bilan isac.}
#'   \item{pluvio}{Les variables appelées par défaut dans le bilan pluvio.}  
#'   ...
#' }
"isac"