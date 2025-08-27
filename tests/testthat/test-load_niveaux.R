

test_that("load_niveaux fonctionne ", {
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
    drv = RMySQL::MySQL(),
    dbname = "archive_IAV",
    host = hostmysql.,
    username = umysql.,
    password = pwdmysql.,
    port = 3306
  )
  res <-
    load_niveaux(
      debut = as.POSIXct(strptime("2010-01-01 00:00:00",
                                  format = "%Y-%m-%d %H:%M:%S")),
      fin = as.POSIXct(strptime("2010-01-10 00:00:00",
                                format = "%Y-%m-%d %H:%M:%S")),
      tags = c(2507, 2508, 2100),
      con = pool
    )
  expect_length(res, 4)
  expect_length(attributes(res)$libelle,3)
  poolClose(pool)
})