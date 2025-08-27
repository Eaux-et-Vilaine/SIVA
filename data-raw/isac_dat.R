debut <- "2020-01-01"  
fin <- "2021-01-01"
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
    drv = RMySQL::MySQL(),
    dbname = "archive_IAV",
    host = hostmysql.,
    username = umysql.,
    password = pwdmysql.,
    port=3306
  )
  
  
isac_dat <-load_isac(
                  debut = as.POSIXct(
                    strptime(debut, format = "%Y-%m-%d")
                  ),
                  fin = as.POSIXct(
                    strptime(fin,   format = "%Y-%m-%d")
                  ),
                  tags = isac$tag[isac$isac],
                  con = pool
                )
usethis::use_data(isac_dat, overwrite = TRUE)


