---
title: "database-tools"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{database-tools}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




```r
library(SIVA)
#> Le chargement a nécessité le package : stacomirtools
#> Le chargement a nécessité le package : RODBC
#> Le chargement a nécessité le package : DBI
#> Le chargement a nécessité le package : pool
```



# tablesiva

La classe tablesiva contient toutes les données pour identifier la table, 
aller la chercher à l'aide de la méthode RequeteODBCwhere du package
stacomirtools.  Elle possède aussi les méthodes décrites ci-dessous.



```r
tablesiva <-
  new(
    "tablesiva",
    debut = as.POSIXct(as.Date("2021-10-25")),
    fin =  as.POSIXct(as.Date("2021-12-01")),
    table = "b_barrage_volet4_hauteur",
    nom = "volet4"
  )
```

# loaddb-method

Il faut configurer le rprofile.site comme suit dans R/R_version/etc/Rprofile.site
pour stocker les user et password lors des tests



```r
 local({
 library(safer)
 mainpass <- "xxxxx"
 umysql<<-encrypt_string("xxxxx",key="mainpass")
 pwdmysql <<- encrypt_string("xxxxxx",key="mainpass")
 hostmysql <<- encrypt_string("xxxxxx",key="mainpass")
cat("creation des passwords umysql, pwdmysql, hostmysql \n")
 })
```

## loaddb-method - tablesiva

La méthode loaddb fonctionne d'abord table par table, ici tablesiva, la classe de connextion, 
va chercher des données pour une table : b_barrage_volet4_hauteur.
Si un tag est nécessaire pour la table il peut être passé également.


```r
# sur mon ordi j'ai des mots de passe chargés au démarrage à partir de Rprofile.site
# Ici test si existent et si oui il faut le main password pour les decrypter, sinon il
# faut les entrer après un prompt du programme. 
if (interactive()){
if (!exists("mainpass")) mainpass <- getPass::getPass(msg = "main password")
if (!exists("hostmysql")) {
  hostmysql. <- getPass::getPass(msg = "Saisir host")
  # ci dessous pour ne pas redemander au prochain tour
  hostmysql <- encrypt_string(string = hostmysql., key = mainpass)
} else {
  hostmysql. <- decrypt_string(string = hostmysql, key = mainpass)
}
if (!exists("pwdmysql")) {
  pwdmysql. <- getPass::getPass(msg = "Saisir password")
  pwdmysql <- encrypt_string(string = pwdmysql., key = mainpass)
}  else {
  # pass should be loaded
  pwdmysql. <- decrypt_string(string = pwdmysql, key = mainpass)
}
if (!exists("umysql")) {
  umysql. <- getPass::getPass(msg = "Saisir user")
  umysql <- encrypt_string(string = umysql., key = mainpass)
} else {
  umysql. <- decrypt_string(string = umysql, key = mainpass)
}

     # attention il faut avaoir définit mainpass <- "xxxxx"
      pool <- pool::dbPool(
          drv = RMariaDB::MariaDB(),
          dbname = "archive_IAV",
          host = hostmysql.,
          username = umysql.,
          password = pwdmysql.,
          port=3306
      )
      tablesiva <-
          new(
              "tablesiva",
              debut = as.POSIXct(as.Date("2021-10-25")),
              fin =  as.POSIXct(as.Date("2021-12-01")),
              table = "b_barrage_volet4_hauteur",
              nom = "volet4"
          )  
 
      tablesiva <- loaddb(tablesiva, pool)
      poolClose(pool)
      knitr::kable(head(tablesiva@rawdata ))
}
#> Table volet4(b_barrage_volet4_hauteur:NA), chargement de 5300 lignes
```



|horodate            | volet4|
|:-------------------|------:|
|2021-10-25 02:10:00 |   4.03|
|2021-10-25 02:20:00 |   4.03|
|2021-10-25 02:30:00 |   4.03|
|2021-10-25 02:40:00 |   4.03|
|2021-10-25 02:50:00 |   4.03|
|2021-10-25 03:00:00 |   4.03|
## loaddb-method - bilansiva

La méthode loaddb fonctionne ensuite pour un ensemble de tables, il s'agit alors d'un objet bilan (bilansiva)



```r

if (interactive()){

      pool <- pool::dbPool(
          drv = RMariaDB::MariaDB(),
          dbname = "archive_IAV",
          host = hostmysql.,
          username = umysql.,
          password = pwdmysql.,
          port=3306
      )

      bil<-new("bilansiva",
          tables=c("b_barrage_debit", 
              "b_pont_de_cran_debit"
          ),
          noms=c(
              "debit_vilaine_estime",
              "debit_moyen_cran"          
          ),
          tags=as.integer(c(NA,1900)),
          daterondes=rep("constant",2),
          debut=as.POSIXct(strptime(paste0(2019,"-05-01 00:00:00"),
                  format="%Y-%m-%d %H:%M:%S")),
          fin=as.POSIXct(strptime(paste0(2019,"-09-01 00:00:00"),
                  format="%Y-%m-%d %H:%M:%S"))
      )
      # the pool connexion will be removed when executing loaddb 
      res <- loaddb(bil, con = pool)

}
#> Table debit_vilaine_estime(b_barrage_debit:NA), chargement de 211787 lignes
```

![plot of chunk example-loaddb-bilansiva-method](figure/example-loaddb-bilansiva-method-1.png)

```
#> Table debit_moyen_cran(b_pont_de_cran_debit:1900), chargement de 17640 lignes
```

![plot of chunk example-loaddb-bilansiva-method](figure/example-loaddb-bilansiva-method-2.png)

```
#> fin des calculs
```
