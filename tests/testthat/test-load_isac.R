test_that("load isac fonctionne", {
      skip_if_not(interactive())
      if (!exists("mainpass"))
        mainpass <- getPass::getPass(msg = "main password")
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
      
      pool <- pool::dbPool(
          drv = RMariaDB::MariaDB(),
          dbname = "archive_IAV",
          host = hostmysql.,
          username = umysql.,
          password = pwdmysql.,
          port = 3306
      )
      res <-
          load_isac(
              debut = as.POSIXct(strptime("2021-01-01 00:00:00",
                      format = "%Y-%m-%d %H:%M:%S")),
              fin = as.POSIXct(strptime("2021-01-10 00:00:00",
                      format = "%Y-%m-%d %H:%M:%S")),
              tags = isac$tag[isac$isac],
              con = pool
          )
      expect_length(res, 9)
      expect_length(attributes(res)$libelle,8)
      pool::poolClose(pool)
})
test_that("load isac fonctionne pour donnees pluvio", {
      skip_if_not(interactive())
      if (!exists("mainpass"))
        mainpass <- getPass::getPass(msg = "main password")
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
      
      pool <- pool::dbPool(
          drv = RMariaDB::MariaDB(),
          dbname = "archive_IAV",
          host = hostmysql.,
          username = umysql.,
          password = pwdmysql.,
          port = 3306
      )
      res <-
          load_isac(
              debut = as.POSIXct(strptime("2020-01-01 00:00:00",
                      format = "%Y-%m-%d %H:%M:%S")),
              fin = as.POSIXct(strptime("2022-01-10 00:00:00",
                      format = "%Y-%m-%d %H:%M:%S")),
              tags = isac$tag[isac$pluvio],
              con = pool
          )
      expect_length(res, 5)
      expect_length(attributes(res)$libelle,4)
      pool::poolClose(pool)
      })