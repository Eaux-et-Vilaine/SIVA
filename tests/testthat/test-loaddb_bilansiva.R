# TODO testme
test_that("loaddb-method for tablesiva fonctionne et retourne un tableau de données", {
      skip_if_not(interactive())
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
      con <- pool::poolCheckout(pool)

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
          debut=as.POSIXct(strptime(paste0(2019,"-09-01 00:00:00"),
                  format="%Y-%m-%d %H:%M:%S")),
          fin=as.POSIXct(strptime(paste0(2019,"-05-01 00:00:00"),
                  format="%Y-%m-%d %H:%M:%S"))
      )
      # the pool connexion will be removed when executing loaddb 
      res <- loaddb(bil, con=con)
      expect_is(res@bilandata,"data.frame")
      dbDisconnect(con)
    })
