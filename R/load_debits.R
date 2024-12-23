#' Chargement des donnees de debits
#'
#' Il faut avoir configuré la connexion à la base, les données de debit peuvent être mise à jour
#'  dans debit voir variable.R. Si certains débits ne sont pas accessibles directement depuis la base, il faut lancer
#' les calculs de débits. 
#'
#' @param debut La date de début format POSIXct ou character "%Y-%m-%d %H:%M:%S
#' @param fin La fin format POSIXct ou character "%Y-%m-%d %H:%M:%S
#' @param tags les tags des données à charger
#' @param con Une connexion pool
#' @return Ub tableau de données des débits chargés directement par loaddb, si
#' certaines variables doivent être recalculées, et les variables debit_recalcule et debit_barQ.
#' @export
#' @importFrom lubridate is.POSIXct
#'
#' @examples
#' \dontrun{
#' load_debits(debut = as.POSIXct(strptime("2010-01-01 00:00:00",
#' format = "%Y-%m-%d %H:%M:%S")),
#'    fin = as.POSIXct(strptime("2010-01-10 00:00:00",
#'    format = "%Y-%m-%d %H:%M:%S")),
#'    tags=c(2515, 1900)
#'    }
load_debits <- function(debut,
                        fin,
                        tags,
                        con) {
  if (!is.POSIXct(debut))
    debut <-
      as.POSIXct(strptime(debut, format = "%Y-%m-%d %H:%M:%S"))
  if (!is.POSIXct(fin))
    fin <- as.POSIXct(strptime(fin, format = "%Y-%m-%d %H:%M:%S"))
  missing <- setdiff(tags, debit$tag)
  if (length(missing) > 0)
    stop(sprintf(
      "Element(s) %s non presents, verifier tags",
      paste(missing, collapse = ", ")
    ))
  debit <- SIVA::debit
  debit_sel <- debit[debit$tag %in% tags,]
  # si la variable n'est pas présente, il faut lancer le calcul des débits
  if (any(is.na(debit_sel$tablehisto)))
    is_calcule_debit <- TRUE
  else
    is_calcule_debit <- FALSE
  debit_sel_sscalc <- debit_sel[!is.na(debit_sel$tablehisto),]
  
  # chargement sans calcul ---------------------------------------

  if (nrow(debit_sel_sscalc) > 0) {
    cat("Chargement depuis base\n")
    bil <- new(
      "bilansiva",
      tables = tolower(debit_sel_sscalc$tablehisto),
      noms = debit_sel_sscalc$code,
      tags = as.integer(debit_sel_sscalc$tag),
      daterondes = rep("constant", nrow(debit_sel_sscalc)),
      debut = debut, #-as.difftime(10,units = "mins")
      fin = fin
    )
    # jeu de données sans calcul
    dat_sscalc <- loaddb(bil, plot = FALSE, con = con)@bilandata
    
    attributes(dat_sscalc)$libelle <-
      c("horodate", debit_sel_sscalc$libelle)

    
  } else {
    dat_sscalc <- NULL
  }
  
  # chargement avec calcul de débit ---------------------------------------
  
  if (is_calcule_debit) {
    cat("Chargement des variables recalcul\u00e9es\n")
    debit_sel_calc <- debit[is.null(debit_sel$tablehisto),]
    # chargement en plus de 10 minutes avant
    # debit_10_min va lancer debit_total qui enlève la première ligne -as.difftime(10, units = "mins")
    debit_barrage <- load_debit_barrage (debut = debut,
                                         fin = fin,
                                         con = con)

    Q10 <- debit_10_min(debit_barrage)
    attributes(Q10)$libelle <-
      c("horodate", "D\u00e9bit recalcul\u00e9 SIVA package")
  } else {
    Q10 <- NULL
  }
  
  if (is.null(dat_sscalc) & (is.null(Q10))) {
    res <- NULL
  } else if (is.null(dat_sscalc)) {
    res <- Q10
  } else if (is.null(Q10))  {
    res <- dat_sscalc
  } else {
    stopifnot(dat_sscalc$horodate[1] == Q10$horodate[1])
    stopifnot(nrow(dat_sscalc) == nrow(Q10))
    res <- dplyr::inner_join(Q10, dat_sscalc) %>% dplyr::slice(-1)
  }
  return(res)
}