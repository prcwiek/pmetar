context("METAR pressure")

x1 <- "EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG"
x2 <- "CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180"
x3 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG="
x4 <- "201905121244 METAR KDCA 121244Z 05010KT 1 3/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T01390122"
x5 <- "201905121244 SPECI KDCA 121244Z 05010KT 2 1/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T11391122"
x6 <- "CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D -SN BLSN BKN008 OVC040 M05/M08 A2992 REFZRA WS RWY36 RMK SF5NS3 SLP134"
x7 <- "METAR KJFK 282355Z AUTO 13009KT 10SM -RA SCT028 SCT035 BKN079 23/20 A2972 RMK T02300200  LTG DSNT SE-SW! MADISHF"
x8 <- "201807141200 METAR EPWA 141200Z 30011G22KT 270V340 9999 -SHRA SCT007 BKN015CB 18/17 Q1011 RESHRA TEMPO BKN007"

x <- c(x1, x2, x3, x4, x5, x6, x7, x8)

dx <- data.frame(metar = x)

test_that("Check pressure in hPa", {
  expect_equal(metar_pressure(x1), 1008)
  expect_equal(metar_pressure(x2, altimeter = FALSE), 1017.61)
  expect_equal(metar_pressure(x3, altimeter = FALSE), 1025)
  expect_equal(metar_pressure(x4), 1008.47)
  expect_equal(metar_pressure(x5, altimeter = FALSE), 1008.47)
  expect_equal(metar_pressure(x6, altimeter = FALSE), 1013.21)
  expect_equal(metar_pressure(x7), 1006.43)
  expect_equal(metar_pressure(x8), 1011)
  expect_equal(metar_pressure(x, altimeter = FALSE), c(1008, 1017.61, 1025, 1008.47, 1008.47, 1013.21, 1006.43, 1011))
  expect_equal(metar_pressure(dx$metar, altimeter = FALSE), c(1008, 1017.61, 1025, 1008.47, 1008.47, 1013.21, 1006.43, 1011))
})


test_that("Check pressure in inHg", {
  expect_equal(metar_pressure(x1, altimeter = TRUE), 29.77)
  expect_equal(metar_pressure(x2, altimeter = TRUE), 30.05)
  expect_equal(metar_pressure(x3, altimeter = TRUE), 30.27)
  expect_equal(metar_pressure(x4, altimeter = TRUE), 29.78)
  expect_equal(metar_pressure(x5, altimeter = TRUE), 29.78)
  expect_equal(metar_pressure(x6, altimeter = TRUE), 29.92)
  expect_equal(metar_pressure(x7, altimeter = TRUE), 29.72)
  expect_equal(metar_pressure(x8, altimeter = TRUE), 29.86)
  expect_equal(metar_pressure(x, altimeter = TRUE), c(29.77, 30.05, 30.27, 29.78, 29.78, 29.92, 29.72, 29.86))
  expect_equal(metar_pressure(dx$metar, altimeter = TRUE), c(29.77, 30.05, 30.27, 29.78, 29.78, 29.92, 29.72, 29.86))
})

x9 <- "EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q10x8 NOSIG"
x10 <- "CYUL 281800Z 13008KT 30SM BKN240 01/M06 AA3005 RMK CI5 SLP180"
x11 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q10251025 NOSIG= NOSIG="

test_that("Incorrect METAR reports", {
  expect_equal(is.na(metar_pressure(x9)), TRUE)
  expect_equal(is.na(metar_pressure(x10)), TRUE)
  expect_equal(is.na(metar_pressure(x11)), TRUE)
  expect_equal(is.na(metar_pressure(x9, altimeter = FALSE)), TRUE)
  expect_equal(is.na(metar_pressure(x10, altimeter = FALSE)), TRUE)
  expect_equal(is.na(metar_pressure(x11, altimeter = FALSE)), TRUE)
})

dx <- data.frame(metar = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11))

test_that("Correct and incorrect METAR reports", {
  expect_equal(metar_pressure(dx$metar, altimeter = FALSE), c(1008, 1017.61, 1025, 1008.47, 1008.47, 1013.21, 1006.43, 1011,
                                                              NA, NA, NA))
})

test_that("Correct and incorrect METAR reports", {
  expect_equal(metar_pressure(dx$metar, altimeter = TRUE), c(29.77, 30.05, 30.27, 29.78, 29.78, 29.92, 29.72, 29.86,
                                                             NA, NA, NA))
})

dxt <- tibble::as_tibble(dx)

test_that("Check invalid input format", {
  expect_error(metar_pressure(dx))
  expect_error(metar_pressure(dxt))
})

