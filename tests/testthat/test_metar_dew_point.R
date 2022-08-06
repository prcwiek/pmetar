context("METAR dew point")

x1 <- "EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG"
x2 <- "CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180"
x3 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG="
x4 <- "201905121244 METAR KDCA 121244Z 05010KT 1 3/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T01390122"
x5 <- "201905121244 SPECI KDCA 121244Z 05010KT 2 1/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T11391122"
x6 <- "CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D -SN BLSN BKN008 OVC040 M05/M08 A2992 REFZRA WS RWY36 RMK SF5NS3 SLP134"
x7 <- "METAR KJFK 282355Z AUTO 13009KT 10SM -RA SCT028 SCT035 BKN079 23/20 A2972 RMK T02300200  LTG DSNT SE-SW! MADISHF"
x71 <- "201901022230 METAR EPWA 022230Z 31019KT 9999 BKN020 M00/M03 Q1014 R33/1///95 NOSIG"

x <- c(x1, x2, x3, x4, x5, x6, x7, x71)

dx <- data.frame(metar = x)

test_that("Check dew point, check = FALSE", {
  expect_equal(metar_dew_point(x1, check = FALSE), -1)
  expect_equal(metar_dew_point(x2, check = FALSE), -6)
  expect_equal(metar_dew_point(x3, check = FALSE), -3)
  expect_equal(metar_dew_point(x4, check = FALSE), 12.2)
  expect_equal(metar_dew_point(x5, check = FALSE), -12.2)
  expect_equal(metar_dew_point(x6, check = FALSE), -8)
  expect_equal(metar_dew_point(x7, check = FALSE), 20)
  expect_equal(metar_dew_point(x71, check = FALSE), -3)
  expect_equal(metar_dew_point(x, check = FALSE), c(-1, -6, -3, 12.2, -12.2, -8, 20, -3))
  expect_equal(metar_dew_point(dx$metar, check = FALSE), c(-1, -6, -3, 12.2, -12.2, -8, 20, -3))
})

test_that("Check dew point, check = TRUE", {
  expect_equal(metar_dew_point(x1, check = TRUE), -1)
  expect_equal(metar_dew_point(x2, check = TRUE), -6)
  expect_equal(metar_dew_point(x3, check = TRUE), -3)
  expect_equal(metar_dew_point(x4, check = TRUE), 12.2)
  expect_equal(metar_dew_point(x5, check = TRUE), -12.2)
  expect_equal(metar_dew_point(x6, check = TRUE), -8)
  expect_equal(metar_dew_point(x7, check = TRUE), 20)
  expect_equal(metar_dew_point(x71, check = TRUE), -3)
  expect_equal(metar_dew_point(x, check = TRUE), c(-1, -6, -3, 12.2, -12.2, -8, 20, -3))
  expect_equal(metar_dew_point(dx$metar, check = TRUE), c(-1, -6, -3, 12.2, -12.2, -8, 20, -3))
})

x8 <- "CYUL 281800Z 13008KT 30SM BKN240 01/M106 A3005 RMK CI5 SLP180"
x9 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK M04/1203 Q1025 NOSIG= NOSIG="
x10 <- "202001010851 METAR KEWR 010851Z 27010KT 10SM BKN210 04/N03 A2969 RMK T003980330 52012"

test_that("Incorrect METAR reports, check = FALSE", {
  expect_equal(is.na(metar_dew_point(x8, check = FALSE)), TRUE)
  expect_equal(is.na(metar_dew_point(x9, check = FALSE)), TRUE)
  expect_equal(is.na(metar_dew_point(x10, check = FALSE)), TRUE)
})

test_that("Incorrect METAR reports, check = TRUE", {
  expect_equal(is.na(metar_dew_point(x8, check = TRUE)), TRUE)
  expect_equal(is.na(metar_dew_point(x9, check = TRUE)), TRUE)
  expect_equal(is.na(metar_dew_point(x10, check = TRUE)), TRUE)
})

dx <- data.frame(metar = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x71))

test_that("Correct and incorrect METAR reports", {
  expect_equal(metar_dew_point(dx$metar),  c(-1, -6, -3, 12.2, -12.2, -8, 20, NA, NA, NA, -3))
})

dxt <- tibble::as_tibble(dx)

test_that("Check invalid input format", {
  expect_error(metar_dew_point(dx))
  expect_error(metar_dew_point(dxt))
})
