context("METAR wind shear")

x1 <- "202003180800 METAR VHHH 180800Z 12009KT 060V150 9999 FEW010 SCT045 22/18 Q1012 WS R07R NOSIG"
x2 <- "CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D -SN BLSN BKN008 OVC040 M05/M08 A2992 REFZRA WS RWY36 RMK SF5NS3 SLP134"
x3 <- "KPIT 091730Z 091818 22020KT 3SM -SHRA BKN020 WS015/30045KT"

x <- c(x1, x2, x3)

dx <- data.frame(metar = x)

test_that("Check wind shear in m/s", {
  expect_equal(metar_windshear(x1, metric = TRUE), "Wind shear runway R07R")
  expect_equal(metar_windshear(x2, metric = TRUE), "Wind shear runway RWY36")
  expect_equal(metar_windshear(x3, metric = TRUE), "Wind shear layer 457.2 m, 23.2 m/s 300 degrees.")
})

test_that("Check wind shear in knots", {
  expect_equal(metar_windshear(x1, metric = FALSE), "Wind shear runway R07R")
  expect_equal(metar_windshear(x2, metric = FALSE), "Wind shear runway RWY36")
  expect_equal(metar_windshear(x3, metric = FALSE), "Wind shear layer 1500 ft, 45 kt 300 degrees.")
})

x4 <- "202003180800 METAR VHHH 180800Z 12009KT 060V150 9999 FEW010 SCT045 22/18 Q1012 WS RR07R NOSIG"
x5 <- "CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D -SN BLSN BKN008 OVC040 M05/M08 A2992 REFZRA WS RWy36 RMK SF5NS3 SLP134"
x6 <- "KPIT 091730Z 091818 22020KT 3SM -SHRA BKN020 WS015/300045KT"

test_that("Incorrect METAR reports", {
  expect_equal(is.na(metar_windshear(x4)), TRUE)
  expect_equal(is.na(metar_windshear(x5)), TRUE)
  expect_equal(is.na(metar_windshear(x6)), TRUE)
  expect_equal(is.na(metar_windshear(x4, metric = FALSE)), TRUE)
  expect_equal(is.na(metar_windshear(x5, metric = FALSE)), TRUE)
  expect_equal(is.na(metar_windshear(x6, metric = FALSE)), TRUE)
})

dx <- data.frame(metar = c(x1, x2, x3, x4, x5, x6))

test_that("Correct and incorrect METAR reports", {
  expect_equal(metar_windshear(dx$metar, metric = TRUE), c("Wind shear runway R07R",
                                                           "Wind shear runway RWY36",
                                                           "Wind shear layer 457.2 m, 23.2 m/s 300 degrees.",
                                                           NA, NA, NA))
  expect_equal(metar_windshear(dx$metar, metric = FALSE), c("Wind shear runway R07R",
                                                           "Wind shear runway RWY36",
                                                           "Wind shear layer 1500 ft, 45 kt 300 degrees.",
                                                           NA, NA, NA))

})

dxt <- tibble::as_tibble(dx)

test_that("Check invalid input format", {
  expect_error(metar_speed(dx))
  expect_error(metar_speed(dxt))
})


