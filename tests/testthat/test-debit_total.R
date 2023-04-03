
test_that("debit_total fonctionne", {
  debit_barrage <- traitement_siva(dat2019)
  Q12345 <- debit_total(param, param0 = param, debit_barrage)
  expect_is(Q12345,"data.frame")
})

test_that("debit total in 2009 (bug #23)", {
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
      # les données annuelles commencent en 2017 pour les données de chaque vanne
      system.time(debit_barrage <-
              load_debit_barrage (
                  debut = as.POSIXct(strptime("2009-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")),
                  fin = as.POSIXct(strptime("2011-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")),
                  con = pool
              ))# 23 s maison
      expect_is(debit_barrage, "data.frame")
      DT <-debit_total(SIVA::param, param0 = SIVA::param,   dat= debit_barrage)
     
      poolClose(pool)
    })