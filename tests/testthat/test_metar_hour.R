context("METAR hour")

x1 <- "EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG"
x2 <- "CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180"
x3 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG="
x4 <- "201905121244 METAR KDCA 121244Z 05010KT 1 3/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T01390122"
x5 <- "201905121244 SPECI KDCA 121244Z 05010KT 2 1/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T01390122"
x6 <- "CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D -SN BLSN BKN008 OVC040 M05/M08 A2992 REFZRA WS RWY36 RMK SF5NS3 SLP134"
x7 <- "METAR KJFK 282355Z AUTO 13009KT 10SM -RA SCT028 SCT035 BKN079 23/20 A2972 RMK T02300200  LTG DSNT SE-SW! MADISHF"

x <- c(x1, x2, x3, x4, x5, x6, x7)

dx <- data.frame(metar = x)

test_that("Check hours", {
  expect_equal(metar_hour(x1), "18:30")
  expect_equal(metar_hour(x2), "18:00")
  expect_equal(metar_hour(x3), "19:30")
  expect_equal(metar_hour(x4), "12:44")
  expect_equal(metar_hour(x5), "12:44")
  expect_equal(metar_hour(x6), "20:00")
  expect_equal(metar_hour(x7), "23:55")
  expect_equal(metar_hour(x), c("18:30", "18:00", "19:30", "12:44", "12:44", "20:00", "23:55"))
  expect_equal(metar_hour(dx$metar), c("18:30", "18:00", "19:30", "12:44", "12:44", "20:00", "23:55"))
})

x8 <- "EPWA 0281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG"
x9 <- "CYU 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180"
x10 <- "201711271930 METAR LEMD 271930 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG="

test_that("Incorrect METAR reports", {
  expect_equal(is.na(metar_hour(x8)), TRUE)
  expect_equal(is.na(metar_hour(x9)), TRUE)
  expect_equal(is.na(metar_hour(x10)), TRUE)
})

dx <- data.frame(metar =  c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))

test_that("Correct and incorrect METAR reports", {
  expect_equal(metar_hour(dx$metar),
               c("18:30", "18:00", "19:30", "12:44", "12:44", "20:00", "23:55", NA, NA, NA))
})
