T
test_that("tablesiva fonctionne", {
  tablesiva <-
    new(
      "tablesiva",
      debut = as.POSIXct(as.Date("2021-10-25")),
      fin =  as.POSIXct(as.Date("2021-12-01")),
      table = "b_barrage_volet4_hauteur",
      nom = "volet4"
    )
  expect_s4_class(tablesiva,  class="tablesiva") 
})
