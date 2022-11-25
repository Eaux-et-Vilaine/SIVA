test_that("loaddb-method fonctionne et retourne un tableau de données", {
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
      options(stacomiR.dbname="archive_IAV", # TODO not used....
          stacomiR.host = hostmysql.,
          stacomiR.password = pwdmysql.,
          stacomiR.user = umysql.,
          stacomiR.ODBClink = "archive_IAV")
      
      tablesiva <-
          new(
              "tablesiva",
              debut = as.POSIXct(as.Date("2021-10-25")),
              fin =  as.POSIXct(as.Date("2021-12-01")),
              table = "b_barrage_volet4_hauteur",
              nom = "volet4"
          )
      res <- loaddb(tablesiva)
      expect_s4_class(res,  class="tablesiva")
      expect_is(res@rawdata,"data.frame")
    })
