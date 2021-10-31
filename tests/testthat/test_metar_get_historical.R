context("METAR get historical")

testthat::skip_if_offline(host = "www.ogimet.com")

test_that("Check incorrect dates with iastate ogimet", {
  expect_error(metar_get_historical(airport = "EPWA",
                                    start_date = "2004-07-01",
                                    end_date = "2004-07-10",
                                    from = "ogimet"))
})

testthat::skip_if_offline(host = "mesonet.agron.iastate.edu")

test_that("Check incorrect dates with iastate option", {
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

