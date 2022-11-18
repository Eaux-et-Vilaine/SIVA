
#TODO change baseODBC ?
# TODO change showmerequest
#' Fonction pour insérer des données dans SIVA. Attention zone de danger !
#'
#' @param data Les données à exporter.
#' @param columns Le nom des colonnes à exporter.
#' @param tabledest Le nom de la table de destination dans la base.
#' @param baseODBC Le lien ODBC vers la base.
#' @param display Faut il afficher les données.
#' @param ... Autres paramètres passés à la fonction.
#'
#' @return nothing
#' @export
#'
#' @examples
#' #TODO
insert_into <- function(data,columns,tabledest,baseODBC,display=FALSE,...) {
  insert_into_sub=function(){
    cat("ecriture dans la base\n")
    progres<-winProgressBar(title = "progression",
        label = "progression %",
        min = 1,
        max = nrow(data), 
        initial = 1,
        width = 400)
    if (display) assign("showmerequest",1,envir=.GlobalEnv)				
    requete=new("RequeteODBC")
    requete@baseODBC=baseODBC
    requete@open=TRUE
    t1<-Sys.time()
    data[,columns][is.na(data[,columns])]<-""
    # this will affect content but not outside the function
    for (i in 1:nrow(data)){
      info<-sprintf("%d%% ecrit",round(100*i/nrow(data)))					
      setWinProgressBar(progres,value=i,label=info)     
      requete@sql=paste( "INSERT INTO ",tabledest,		
          "(",paste(columns,sep="",collapse=","),
          ")VALUES (",paste(data[i,columns],sep="",collapse=","),
          ");",sep="")
      requete<-connect(requete)
      
    } # end for
    print(Sys.time()-t1)
    if (display) remove("showmerequest",envir=.GlobalEnv)
    odbcCloseAll()
    close(progres)
  } 			
  # main part of the function below
  if (!all(columns%in%names(data))) stop(paste("toutes les colonnes ne sont pas dans les données, il faut les créer avant de lancer cette merveilleuse fonction"))
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
      stop("Les colonnes insérées ne sont pas la table de la base de données.")	
    }	
  }
  return(invisible(NULL))
}	

