context("METAR get historical")

# test_that("Check if METAR reports for the year 2019 from EPWA are downloaded correctly", {
#   skip_on_cran()
#   path_data <- file.path(devtools::inst(name="pmetar"), "test_data", "EPWA_2019.RData")
#   load(path_data)
#   METARs_downloaded <- metar_get_historical(airport = "EPWA",
#                                             start_date = "2019-01-01",
#                                             end_date = "2019-12-31",
#                                             from = "iastate")
#   expect_equal(EPWA_2019, METARs_downloaded)
# })

# test_that("Check if METAR reports for January 2019 from FRA are downloaded correctly", {
#   skip_on_cran()
#   path_data <- file.path(devtools::inst(name="pmetar"), "test_data", "FRA_2019_01.RData")
#   load(path_data)
#   METARs_downloaded <- metar_get_historical(airport = "FRA",
#                                             start_date = "2019-01-01",
#                                             end_date = "2019-01-31",
#                                             from = "ogimet")
#   expect_equal(FRA_2019_01, METARs_downloaded)
# })

test_that("Check incorrect dates", {
  expect_error(metar_get_historical(airport = "EPWA",
                                    start_date = "2019-01-01",
                                    end_date = "2018-12-31",
                                    from = "iastate"))
  expect_error(metar_get_historical(airport = "EPWA",
                                    start_date = "2019/01/01",
                                    end_date = "2018/12/31",
                                    from = "iastate"))
  expect_error(metar_get_historical(airport = "FRA",
                                    start_date = "2000-05-01",
                                    end_date = "2000-05-31",
                                    from = "iastate"))
  expect_error(metar_get_historical(airport = "EPWA",
                                    start_date = "2004-07-01",
                                    end_date = "2004-07-10",
                                    from = "ogimet"))
})

test_that("Check incorrect airport code", {
  expect_error(metar_get_historical(airport = "EPWAa",
                                    start_date = "2019-01-01",
                                    end_date = "2019-12-31",
                                    from = "iastate"))
  expect_error(metar_get_historical(airport = "ABCD",
                                    start_date = "2019-01-01",
                                    end_date = "2019-12-31",
                                    from = "iastate"))
  expect_error(metar_get_historical(airport = "c1C",
                                    start_date = "2019-01-01",
                                    end_date = "2019-12-31",
                                    from = "iastate"))
  expect_error(metar_get_historical(airport = "A",
                                    start_date = "2019-01-01",
                                    end_date = "2019-12-31",
                                    from = "iastate"))
})

