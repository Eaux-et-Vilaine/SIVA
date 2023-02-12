test_that("load_vega fonctionne", {
      skip_if_not(interactive())
      skip_if_not(exists("apikey"))
      res <- load_vega("Trevelo", apiKey=apikey )
      testthat::expect_is(res, "data.frame")
})
