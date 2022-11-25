
#TODO change baseODBC ?
# TODO change showmerequest
#' Fonction pour insérer des données dans SIVA. Attention zone de danger !
#'
#' @param data Les données à exporter.
#' @param columns Le nom des colonnes à exporter.
#' @param tabledest Le nom de la table de destination dans la base.
#' @param baseODBC Le lien ODBC vers la base.
#' @param ... Autres paramètres passés à la fonction.
#'
#' @return nothing
#' @export
#'
#' @examples
#' #TODO
#' @importFrom utils setWinProgressBar
#' @importFrom RODBC odbcCloseAll
#' @importFrom utils winProgressBar
insert_into <- function(data,columns,tabledest,baseODBC,...) {
  insert_into_sub=function(){
    cat("ecriture dans la base\n")
    progres<-txtProgressBar(
        min = 1,
        max = nrow(data), 
        style=3,
        width = nrow(data))
    requete=new("RequeteODBC")
    requete@baseODBC=baseODBC
    requete@open=TRUE
    t1<-Sys.time()
    data[,columns][is.na(data[,columns])]<-""
    # this will affect content but not outside the function
    for (i in 1:nrow(data)){
      info<-sprintf("%d%% ecrit",round(100*i/nrow(data)))					
      setTxtProgressBar(progres,value=i)     
      requete@sql=paste( "INSERT INTO ",tabledest,		
          "(",paste(columns,sep="",collapse=","),
          ")VALUES (",paste(data[i,columns],sep="",collapse=","),
          ");",sep="")
      requete<-connect(requete)
      
    } # end for
    print(Sys.time()-t1)
    odbcCloseAll()
    close(progres)
  } 			
  # main part of the function below
  if (!all(columns%in%names(data))) stop(paste("toutes les colonnes ne sont pas dans les donn\u00e9es, il faut les cr\u00e9er avant de lancer cette merveilleuse fonction"))
  requete=new("RequeteODBC")
  requete@baseODBC=baseODBC
  requete@sql=paste("SELECT * FROM ",tabledest, " LIMIT 1000",sep="")
  requete<-connect(requete)
  if (nrow(requete@query)>0){
    insert_into_sub()
  } else {
    if (all(columns%in%names(requete@query))) {
      insert_into_sub()
    }else {
      stop("Les colonnes ins\u00e9r\u00e9es ne sont pas la table de la base de donn\u00e9es.")	
    }	
  }
  return(invisible(NULL))
}	

