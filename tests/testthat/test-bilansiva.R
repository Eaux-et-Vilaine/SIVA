
test_that("bilansiva-method fonctionne ", {
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
           debut=as.POSIXct(strptime(paste0(2019,"-09-01 00:00:00"),format="%Y-%m-%d %H:%M:%S")),
           fin=as.POSIXct(strptime(paste0(2020,"-05-01 00:00:00"),format="%Y-%m-%d %H:%M:%S"))
  )

  expect_s4_class(bil,  class="bilansiva")

})
