context("METAR wind speed")

x1 <- "EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG"
x2 <- "CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180"
x3 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG="
x4 <- "201905121244 METAR KDCA 121244Z 05010KT 1 3/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T01390122"
x5 <- "201905121244 SPECI KDCA 121244Z 05010KT 2 1/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T11391122"
x6 <- "CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D -SN BLSN BKN008 OVC040 M05/M08 A2992 REFZRA WS RWY36 RMK SF5NS3 SLP134"
x7 <- "METAR KJFK 282355Z AUTO 13009KT 10SM -RA SCT028 SCT035 BKN079 23/20 A2972 RMK T02300200  LTG DSNT SE-SW MADISHF"
x8 <- "201807141200 METAR EPWA 141200Z 30011G22KT 270V340 9999 -SHRA SCT007 BKN015CB 18/17 Q1011 RESHRA TEMPO BKN007"
x9 <- "EPKK 141730Z VRB01KT CAVOK 21/16 Q1028"
x91 <- "202004281415 METAR KEWR 281415Z AUTO VRB05G14KT 10SM CLR 12/00 A3013"
x92 <- "EPWA 281830Z 240P99KT 9999 SCT037 03/M01 Q1008 NOSIG"
x93 <- "EPWA 281830Z 240P49MPS 9999 SCT037 03/M01 Q1008 NOSIG"

x <- c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x91, x92, x93)

dx <- data.frame(metar = x)

test_that("Check wind speed in knots", {
  expect_equal(metar_speed(x1, metric = FALSE, check = FALSE), 9)
  expect_equal(metar_speed(x2, metric = FALSE, check = FALSE), 8)
  expect_equal(metar_speed(x3, metric = FALSE, check = FALSE), 2)
  expect_equal(metar_speed(x4, metric = FALSE, check = FALSE), 10)
  expect_equal(metar_speed(x5, metric = FALSE, check = FALSE), 10)
  expect_equal(metar_speed(x6, metric = FALSE, check = FALSE), 15)
  expect_equal(metar_speed(x7, metric = FALSE, check = FALSE), 9)
  expect_equal(metar_speed(x8, metric = FALSE, check = FALSE), 11)
  expect_equal(metar_speed(x9, metric = FALSE, check = FALSE), 1)
  expect_equal(metar_speed(x91, metric = FALSE, check = FALSE), 5)
  expect_equal(metar_speed(x92, metric = FALSE, check = FALSE), 100)
  expect_equal(metar_speed(x93, metric = FALSE, check = FALSE), 100)
  expect_equal(metar_speed(x, metric = FALSE, check = FALSE), c(9, 8, 2, 10, 10, 15, 9, 11, 1, 5, 100, 100))
  expect_equal(metar_speed(dx$metar, metric = FALSE, check = FALSE), c(9, 8, 2, 10, 10, 15, 9, 11, 1, 5, 100, 100))
})


test_that("Check wind speed in m/s", {
  expect_equal(metar_speed(x1, metric = TRUE, check = FALSE), 9 * 0.5144447)
  expect_equal(metar_speed(x2, metric = TRUE, check = FALSE), 8 * 0.5144447)
  expect_equal(metar_speed(x3, metric = TRUE, check = FALSE), 2 * 0.5144447)
  expect_equal(metar_speed(x4, metric = TRUE, check = FALSE), 10 * 0.5144447)
  expect_equal(metar_speed(x5, metric = TRUE, check = FALSE), 10 * 0.5144447)
  expect_equal(metar_speed(x6, metric = TRUE, check = FALSE), 15 * 0.5144447)
  expect_equal(metar_speed(x7, metric = TRUE, check = FALSE), 9 * 0.5144447)
  expect_equal(metar_speed(x8, metric = TRUE, check = FALSE), 11 * 0.5144447)
  expect_equal(metar_speed(x9, metric = TRUE, check = FALSE), 1 * 0.5144447)
  expect_equal(metar_speed(x91, metric = TRUE, check = FALSE), 5 * 0.5144447)
  expect_equal(metar_speed(x92, metric = TRUE, check = FALSE), 50)
  expect_equal(metar_speed(x93, metric = TRUE, check = FALSE), 50)
  expect_equal(metar_speed(x, metric = TRUE, check = FALSE), 
               c(9 * 0.5144447, 8 * 0.5144447, 2 * 0.5144447,
                 10 * 0.5144447, 10 * 0.5144447, 15 * 0.5144447,
                 9 * 0.5144447, 11 * 0.5144447, 1 * 0.5144447,
                 5 * 0.5144447, 50, 50))
  expect_equal(metar_speed(dx$metar, metric = TRUE, check = FALSE),
               c(9 * 0.5144447, 8 * 0.5144447, 2 * 0.5144447,
                 10 * 0.5144447, 10 * 0.5144447, 15 * 0.5144447,
                 9 * 0.5144447, 11 * 0.5144447, 1 * 0.5144447,
                 5 * 0.5144447, 50, 50))
})

x8 <- "EPWA 281830Z 1800xKT 140V200 9999 SCT037 03/M01 Q1008 NOSIG"
x9 <- "CYUL 281800Z 8KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180"
x10 <- "201711271930 METAR LEMD 271930Z KTKT CAVOK 04/M03 Q1025 NOSIG= NOSIG="

test_that("Incorrect METAR reports, check = FALSE", {
  expect_equal(is.na(metar_speed(x8, check = FALSE)), TRUE)
  expect_equal(is.na(metar_speed(x9, check = FALSE)), TRUE)
  expect_equal(is.na(metar_speed(x10, check = FALSE)), TRUE)
  expect_equal(is.na(metar_speed(x8, metric = FALSE, check = FALSE)), TRUE)
  expect_equal(is.na(metar_speed(x9, metric = FALSE, check = FALSE)), TRUE)
  expect_equal(is.na(metar_speed(x10, metric = FALSE, check = FALSE)), TRUE)
})

#res <- NA

test_that("Incorrect METAR reports, check = TRUE", {
  expect_equal(is.na(metar_speed(x8, check = TRUE)), TRUE)
  expect_equal(is.na(metar_speed(x9, check = TRUE)), TRUE)
  expect_equal(is.na(metar_speed(x10, check = TRUE)), TRUE)
  expect_equal(is.na(metar_speed(x8, metric = FALSE, check = TRUE)), TRUE)
  expect_equal(is.na(metar_speed(x9, metric = FALSE, check = TRUE)), TRUE)
  expect_equal(is.na(metar_speed(x10, metric = FALSE, check = TRUE)), TRUE)
})

dx <- data.frame(metar = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))

test_that("Correct and incorrect METAR reports", {
  expect_equal(metar_speed(dx$metar, metric = TRUE, check = FALSE), 
               c(9 * 0.5144447, 8 * 0.5144447, 2 * 0.5144447,
                 10 * 0.5144447, 10 * 0.5144447, 15 * 0.5144447,
                 9 * 0.5144447, NA, NA, NA))
})

test_that("Correct and incorrect METAR reports", {
  expect_equal(metar_speed(dx$metar[1:7], metric = FALSE, check = TRUE),
               c(9, 8, 2, 10, 10, 15, 9))
  expect_equal(is.na(metar_speed(dx$metar[8:10], metric = FALSE, check = TRUE)),
               c(TRUE, TRUE, TRUE))
})

dxt <- tibble::as_tibble(dx)

test_that("Check invalid input format", {
  expect_error(metar_speed(dx))
  expect_error(metar_speed(dxt))
})
