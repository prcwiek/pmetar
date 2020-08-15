context("METAR wind gust")

x1 <- "EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG"
x2 <- "CYUL 101900Z 27015G25KT 15SM DRSN SCT028 BKN090 OVC110 M04/M10 A2973 RMK"
x3 <- "METAR KEWR 051251Z 29016G28KT 10SM BKN045 03/M06 A2979 RMK PK WND 30028/1243 SLP088 T00331056"
x4 <- "SPECI KEWR 071625Z AUTO /////G31KT 6SM HZ FEW004 BKN025 OVC035 16/12 A2879 RMK T01600120 MADISHF"
x5 <- "201905121244 SPECI KDCA 121244Z 05010KT 2 1/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T11391122"
x6 <- "METAR CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D -SN BLSN BKN008 OVC040 M05/M08 A2992 REFZRA WS RWY36 RMK SF5NS3 SLP134"
x7 <- "METAR KJFK 282355Z AUTO 13009KT 10SM -RA SCT028 SCT035 BKN079 23/20 A2972 RMK T02300200  LTG DSNT SE-SW! MADISHF"
x8 <- "201807141200 SPECI EPWA 141200Z 30011G22KT 270V340 9999 -SHRA SCT007 BKN015CB 18/17 Q1011 RESHRA TEMPO BKN007"

x <- c(x1, x2, x3, x4, x5, x6, x7, x8)

dx <- data.frame(metar = x)

test_that("Check wind gust in knots", {
  expect_equal(is.na(metar_gust(x1, metric = FALSE)), TRUE)
  expect_equal(metar_gust(x2, metric = FALSE), 25)
  expect_equal(metar_gust(x3, metric = FALSE), 28)
  expect_equal(metar_gust(x4, metric = FALSE), 31)
  expect_equal(is.na(metar_gust(x5, metric = FALSE)), TRUE)
  expect_equal(metar_gust(x6, metric = FALSE), 25)
  expect_equal(is.na(metar_gust(x7, metric = FALSE)), TRUE)
  expect_equal(metar_gust(x8, metric = FALSE), 22)
  expect_equal(metar_gust(x, metric = FALSE), c(NA, 25, 28, 31, NA, 25, NA, 22))
  expect_equal(metar_gust(dx$metar, metric = FALSE), c(NA, 25, 28, 31, NA, 25, NA, 22))
})


test_that("Check wind gust in m/s", {
  expect_equal(is.na(metar_gust(x1, metric = TRUE)), TRUE)
  expect_equal(metar_gust(x2, metric = TRUE), 25 * 0.5144447)
  expect_equal(metar_gust(x3, metric = TRUE), 28 * 0.5144447)
  expect_equal(metar_gust(x4, metric = TRUE), 31 * 0.5144447)
  expect_equal(is.na(metar_gust(x5, metric = TRUE)), TRUE)
  expect_equal(metar_gust(x6, metric = TRUE), 25 * 0.5144447)
  expect_equal(is.na(metar_gust(x7, metric = TRUE)), TRUE)
  expect_equal(metar_gust(x8, metric = TRUE), 22 * 0.5144447)
  expect_equal(metar_gust(x, metric = TRUE), c(NA, 25 * 0.5144447, 28 * 0.5144447,
                                               31 * 0.5144447, NA, 25 * 0.5144447,
                                               NA, 22 * 0.5144447))
  expect_equal(metar_gust(dx$metar, metric = TRUE), c(NA, 25 * 0.5144447, 28 * 0.5144447,
                                                      31 * 0.5144447, NA, 25 * 0.5144447,
                                                      NA, 22 * 0.5144447))
})



x9 <- "EPWA 281830Z 18009GKT 140V200 9999 SCT037 03/M01 Q1008 NOSIG"
x10 <- "CYUL 101900Z 27015G2aKT 15SM DRSN SCT028 BKN090 OVC110 M04/M10 A2973 RMK"
x11 <- "METAR KEWR 051251Z 29016G2822KT 10SM BKN045 03/M06 A2979 RMK PK WND 30028/1243 SLP088 T00331056"

test_that("Incorrect METAR reports", {
  expect_equal(is.na(metar_gust(x9)), TRUE)
  expect_equal(is.na(metar_gust(x10)), TRUE)
  expect_equal(is.na(metar_gust(x11)), TRUE)
  expect_equal(is.na(metar_gust(x9, metric = FALSE)), TRUE)
  expect_equal(is.na(metar_gust(x10, metric = FALSE)), TRUE)
  expect_equal(is.na(metar_gust(x11, metric = FALSE)), TRUE)
})

dx <- data.frame(metar = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11))

test_that("Correct and incorrect METAR reports", {
  expect_equal(metar_gust(dx$metar, metric = FALSE), c(NA, 25, 28, 31, NA, 25, NA, 22,
                                                       NA, NA, NA))
})

test_that("Correct and incorrect METAR reports", {
  expect_equal(metar_gust(dx$metar, metric = TRUE), c(NA, 25 * 0.5144447, 28 * 0.5144447,
                                                      31 * 0.5144447, NA, 25 * 0.5144447,
                                                      NA, 22 * 0.5144447, NA, NA, NA))
})

