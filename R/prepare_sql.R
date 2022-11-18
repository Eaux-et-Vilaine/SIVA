#' Prepare les données pour l'insertion (cast au bon format)
#'
#' @param data Un data.frame.
#'
#' @return data Un data.frame.
#' @export
#'
#' @examples
#' #TODO
prepare_sql <- function(data) {
  # c'est nul mais ni ddply ni apply ne me permettent de faire ça
  for (i in 1:ncol(data)) {
    for (j in 1:nrow(data)) {
      data[j, i] <- gsub("'", "''", data[j, i])
    }
  }
  for (i in 1:ncol(data)) {
    if (class(data[1, i])[1] == "POSIXct") 	{
      data[, i] <- as.character(data[, i])
    }
    if (class(data[1, i])[1] == "character")	{
      data[!is.na(data[, i]), i] <- sQuote(data[!is.na(data[, i]), i])
      data[is.na(data[, i]), i] <- 'NULL'
    }
    if (class(data[1, i])[1] == "integer") {
      data[is.na(data[, i]), i] <- 'NULL'
    }
    
  }
  
  return(data)
}
