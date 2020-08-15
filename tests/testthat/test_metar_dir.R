context("METAR wind direction")

x1 <- "EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG"
x2 <- "CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180"
x3 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG="
x4 <- "201905121244 METAR KDCA 121244Z 05010KT 1 3/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T01390122"
x5 <- "201905121244 SPECI KDCA 121244Z 05010KT 2 1/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T11391122"
x6 <- "CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D -SN BLSN BKN008 OVC040 M05/M08 A2992 REFZRA WS RWY36 RMK SF5NS3 SLP134"
x7 <- "METAR KJFK 282355Z AUTO 13009KT 10SM -RA SCT028 SCT035 BKN079 23/20 A2972 RMK T02300200  LTG DSNT SE-SW! MADISHF"

x <- c(x1, x2, x3, x4, x5, x6, x7)

dx <- data.frame(metar = x)

test_that("Check wind direction", {
  expect_equal(metar_dir(x1), "180, variable from 140 to 200")
  expect_equal(metar_dir(x2), "130")
  expect_equal(metar_dir(x3), "20")
  expect_equal(metar_dir(x4), "50")
  expect_equal(metar_dir(x5), "50")
  expect_equal(metar_dir(x6), "300")
  expect_equal(metar_dir(x7), "130")
  expect_equal(metar_dir(x), c("180, variable from 140 to 200", "130", "20", "50", "50", "300", "130"))
  expect_equal(metar_dir(dx$metar), c("180, variable from 140 to 200", "130", "20", "50", "50", "300", "130"))
})

test_that("Check wind direction, numeric_only", {
  expect_equal(metar_dir(x1, numeric_only = TRUE), 180)
  expect_equal(metar_dir(x2, numeric_only = TRUE), 130)
  expect_equal(metar_dir(x3, numeric_only = TRUE), 20)
  expect_equal(metar_dir(x4, numeric_only = TRUE), 50)
  expect_equal(metar_dir(x5, numeric_only = TRUE), 50)
  expect_equal(metar_dir(x6, numeric_only = TRUE), 300)
  expect_equal(metar_dir(x7, numeric_only = TRUE), 130)
  expect_equal(metar_dir(x, numeric_only = TRUE), c(180, 130, 20, 50, 50, 300, 130))
  expect_equal(metar_dir(dx$metar, numeric_only = TRUE), c(180, 130, 20, 50, 50, 300, 130))
})
x8 <- "EPWA 281830Z 1118009KT 140V2000 9999 SCT037 03/M01 Q1008 NOSIG"
x9 <- "CYUL 281800Z 8KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180"
x10 <- "201711271930 METAR LEMD 271930Z 0c002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG="


test_that("Incorrect METAR reports", {
  expect_equal(metar_dir(x8), "180, variable from 140 to 200")
  expect_equal(metar_dir(x9), "")
  expect_equal(metar_dir(x10), "")
})

test_that("Incorrect METAR reports, numeric_only", {
  expect_equal(metar_dir(x8, numeric_only = TRUE), 180)
  expect_equal(is.na(metar_dir(x9, numeric_only = TRUE)), TRUE)
  expect_equal(is.na(metar_dir(x10, numeric_only = TRUE)), TRUE)
})


dx <- data.frame(metar = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))

test_that("Correct and incorrect METAR reports", {
  expect_equal(metar_dir(dx$metar),  c("180, variable from 140 to 200", "130", "20", "50", "50", "300", "130",
                                       "180, variable from 140 to 200", "", ""))
})

test_that("Correct and incorrect METAR reports, numeric_only", {
  expect_equal(metar_dir(dx$metar, numeric_only = TRUE),  c(180, 130, 20, 50, 50, 300, 130, 180, NA, NA))
})

dxt <- tibble::as_tibble(dx)

test_that("Check invalid input format", {
  expect_error(metar_dir(dx))
  expect_error(metar_dir(dxt))
})
