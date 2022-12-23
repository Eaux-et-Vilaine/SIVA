

test_that("load_debits fonctionne (variables sans recalcul)", {
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
    load_debits(
      debut = as.POSIXct(strptime("2020-01-01 00:00:00",
                                  format = "%Y-%m-%d %H:%M:%S")),
      fin = as.POSIXct(strptime("2020-01-10 00:00:00",
                                format = "%Y-%m-%d %H:%M:%S")),
      tags = c(2515, 1900, 3000, 3100),
      con = pool
    )
  expect_length(res, 3)
  expect_length(res$dat_sscalc, 5)
  expect_length(attributes(res$dat_sscalc)$libelle,5)
  
  poolClose(pool)
  
  
})
  
  
test_that("load_debits fonctionne (variables nécessitant un recalcul des débits)", {
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
    load_debits(
      debut = as.POSIXct(strptime("2020-01-01 00:00:00",
                                  format = "%Y-%m-%d %H:%M:%S")),
      fin = as.POSIXct(strptime("2020-01-10 00:00:00",
                                format = "%Y-%m-%d %H:%M:%S")),
      tags = c(40001,40002),
      con = pool
    )
  poolClose(pool)
  expect_length(res, 3)
  expect_length(res$Q12345, 25)
})