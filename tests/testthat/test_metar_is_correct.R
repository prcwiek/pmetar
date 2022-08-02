context("METAR is correct")

x1 <- "EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG"
x2 <- "CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180"
x3 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG="
x4 <- "201905121244 METAR KDCA 121244Z 05010KT 1 3/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T01390122"
x5 <- "201905121244 SPECI KDCA 121244Z 05010KT 2 1/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T01390122"
x6 <- "CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D -SN BLSN BKN008 OVC040 M05/M08 A2992 REFZRA WS RWY36 RMK SF5NS3 SLP134"
x7 <- "202103251800 METAR COR NFTL 251800Z 00000KT 9999 SCT017TCU BKN290 25/25 Q1014"
x8 <- "202103251800 SPECI COR NFTL 251800Z 00000KT 9999 SCT017TCU BKN290 25/25 Q1014"
x9 <- "METAR KJFK 282355Z AUTO 13009KT 10SM -RA SCT028 SCT035 BKN079 23/20 A2972 RMK T02300200  LTG DSNT SE-SW! MADISHF"

x <- c(x1, x2, x3, x4, x5, x6, x7, x8, x9)

dx <- data.frame(metar = x)

test_that("Check correctness of correct METAR reports",{
  expect_equal(metar_is_correct(x1), TRUE)
  expect_equal(metar_is_correct(x2), TRUE)
  expect_equal(metar_is_correct(x3), TRUE)
  expect_equal(metar_is_correct(x4), TRUE)
  expect_equal(metar_is_correct(x5), TRUE)
  expect_equal(metar_is_correct(x6), TRUE)
  expect_equal(metar_is_correct(x7), TRUE)
  expect_equal(metar_is_correct(x8), TRUE)
  expect_equal(metar_is_correct(x9), TRUE)
  expect_equal(metar_is_correct(x), rep(TRUE, 9))
  expect_equal(metar_is_correct(dx$metar), rep(TRUE, 9))
})

x10 <- "EPWA281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG"
x11 <- "201711271930 METARLEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG="
x12 <- "201711271930 METAR abcE 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG="
x13 <- "201711271930 METAR lemd 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG="
x14 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG* NOSIG="
x15 <- "201711271930 METAR LEMD 27.1930Z 02002KT CAVOK 04/M03 Q1025NOSIG="
x16 <- "METAR SPECI CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180"
x17 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK 04//M03 Q1025 NOSIG="
x18 <- "201711271930 METAR LEMD 271930Z ! 02002KT CAVOK 04/M03 Q1025 NOSIG="
x19 <- "201711271930 METAR LEMD 271930Z 0200?KT CAVOK 04/M03 Q1025 NOSIG="
x20 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025.0 NOSIG="
x21 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK 04,0/M03 Q1025 NOSIG="
x22 <- "201711271930 METAR LEMD 271930Z 02002KT; CAVOK; 04/M03 Q1025 NOSIG="
x23 <- "201711271930 METAR LEMD: 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG="
x24 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG=*"
x25 <- "201711271930 METAR LEMD 271930Z 02002KT #CAVOK 04/M03 Q1025 NOSIG="
x26 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK 04&M03 Q1025 NOSIG="
x27 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG='"

x <- c(x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27)

dx <- data.frame(metar = x)

test_that("Check correctness of incorrect METAR reports",{
  expect_equal(metar_is_correct(x10), FALSE)
  expect_equal(metar_is_correct(x11), FALSE)
  expect_equal(metar_is_correct(x12), FALSE)
  expect_equal(metar_is_correct(x13), FALSE)
  expect_equal(metar_is_correct(x14), FALSE)
  expect_equal(metar_is_correct(x15), FALSE)
  expect_equal(metar_is_correct(x16), FALSE)
  expect_equal(metar_is_correct(x17), FALSE)
  expect_equal(metar_is_correct(x18), FALSE)
  expect_equal(metar_is_correct(x19), FALSE)
  expect_equal(metar_is_correct(x20), FALSE)
  expect_equal(metar_is_correct(x21), FALSE)
  expect_equal(metar_is_correct(x22), FALSE)
  expect_equal(metar_is_correct(x23), FALSE)
  expect_equal(metar_is_correct(x24), FALSE)
  expect_equal(metar_is_correct(x25), FALSE)
  expect_equal(metar_is_correct(x26), FALSE)
  expect_equal(metar_is_correct(x27), FALSE)
  expect_equal(metar_is_correct(x), rep(FALSE, 18))
  expect_equal(metar_is_correct(dx$metar), rep(FALSE, 18))
})

dxt <- tibble::as_tibble(dx)

test_that("Check invalid input format", {
  expect_error(metar_is_correct(dx))
  expect_error(metar_is_correct(dxt))
})


