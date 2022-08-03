context("METAR temperature")

x1 <- "EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG"
x2 <- "CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180"
x3 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG="
x4 <- "201905121244 METAR KDCA 121244Z 05010KT 1 3/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T01390122"
x5 <- "201905121244 SPECI KDCA 121244Z 05010KT 2 1/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T11390122"
x6 <- "CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D -SN BLSN BKN008 OVC040 M05/M08 A2992 REFZRA WS RWY36 RMK SF5NS3 SLP134"
x7 <- "METAR KJFK 282355Z AUTO 13009KT 10SM -RA SCT028 SCT035 BKN079 23/20 A2972 RMK T02300200  LTG DSNT SE-SW! MADISHF"

x <- c(x1, x2, x3, x4, x5, x6, x7)

dx <- data.frame(metar = x)

test_that("Check temperature, check = FALSE", {
  expect_equal(metar_temp(x1, check = FALSE), 3)
  expect_equal(metar_temp(x2, check = FALSE), 1)
  expect_equal(metar_temp(x3, check = FALSE), 4)
  expect_equal(metar_temp(x4, check = FALSE), 13.9)
  expect_equal(metar_temp(x5, check = FALSE), -13.9)
  expect_equal(metar_temp(x6, check = FALSE), -5)
  expect_equal(metar_temp(x7, check = FALSE), 23)
  expect_equal(metar_temp(x, check = FALSE), c(3, 1, 4, 13.9, -13.9, -5, 23))
  expect_equal(metar_temp(dx$metar, check = FALSE), c(3, 1, 4, 13.9, -13.9, -5, 23))
})

test_that("Check temperature, check = TRUE", {
  expect_equal(metar_temp(x1, check = TRUE), 3)
  expect_equal(metar_temp(x2, check = TRUE), 1)
  expect_equal(metar_temp(x3, check = TRUE), 4)
  expect_equal(metar_temp(x4, check = TRUE), 13.9)
  expect_equal(metar_temp(x5, check = TRUE), -13.9)
  expect_equal(metar_temp(x6, check = TRUE), -5)
  expect_equal(metar_temp(x7, check = TRUE), 23)
  expect_equal(metar_temp(x, check = TRUE), c(3, 1, 4, 13.9, -13.9, -5, 23))
  expect_equal(metar_temp(dx$metar, check = TRUE), c(3, 1, 4, 13.9, -13.9, -5, 23))
})

x8 <- "CYUL 281800Z 13008KT 30SM BKN240 101/M06 A3005 RMK CI5 SLP180"
x9 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK M904/M03 Q1025 NOSIG= NOSIG="
x10 <- "202001010851 METAR KEWR 010851Z 27010KT 10SM FEW030 BKN070 BKN100 BKN210 04/N03 A2969 RMK SLP054 T40391033 52012"

test_that("Incorrect METAR reports, check = FALSE", {
  expect_equal(is.na(metar_temp(x8, check = FALSE)), TRUE)
  expect_equal(is.na(metar_temp(x9, check = FALSE)), TRUE)
  expect_equal(is.na(metar_temp(x10, check = FALSE)), TRUE)
})

test_that("Incorrect METAR reports, check = TRUE", {
  expect_equal(is.na(metar_temp(x8, check = TRUE)), TRUE)
  expect_equal(is.na(metar_temp(x9, check = TRUE)), TRUE)
  expect_equal(is.na(metar_temp(x10, check = TRUE)), TRUE)
})

dx <- data.frame(metar = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))

test_that("Correct and incorrect METAR reports", {
  expect_equal(metar_temp(dx$metar),  c(3, 1, 4, 13.9, -13.9, -5, 23, NA, NA, NA))
})

dxt <- tibble::as_tibble(dx)

test_that("Check invalid input format", {
  expect_error(metar_temp(dx))
  expect_error(metar_temp(dxt))
})

