context("METAR visibility")

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

test_that("Check visibility in meters", {
  expect_equal(metar_visibility(x1, metric = TRUE), "9999")
  expect_equal(metar_visibility(x2, metric = TRUE), "48280.32")
  expect_equal(metar_visibility(x3, metric = TRUE), "Ceiling And Visibility OK")
  expect_equal(metar_visibility(x4, metric = TRUE), "2816.35")
  expect_equal(metar_visibility(x5), "3621.02")
  expect_equal(metar_visibility(x6), "1207.01")
  expect_equal(metar_visibility(x7), "16093.44")
  expect_equal(metar_visibility(x8), "9999")
  expect_equal(metar_visibility(x), c("9999", "48280.32", "Ceiling And Visibility OK", "2816.35", "3621.02", "1207.01", "16093.44", "9999"))
  expect_equal(metar_visibility(dx$metar), c("9999", "48280.32", "Ceiling And Visibility OK", "2816.35", "3621.02", "1207.01", "16093.44", "9999"))
})

test_that("Check visibility in meters, numeric values only", {
  expect_equal(metar_visibility(x1, metric = TRUE, numeric_values_only = TRUE), 9999)
  expect_equal(metar_visibility(x2, metric = TRUE, numeric_values_only = TRUE), 48280.32)
  expect_equal(metar_visibility(x3, metric = TRUE, numeric_values_only = TRUE), 10000)
  expect_equal(metar_visibility(x4, metric = TRUE, numeric_values_only = TRUE), 2816.35)
  expect_equal(metar_visibility(x5, numeric_values_only = TRUE), 3621.02)
  expect_equal(metar_visibility(x6, numeric_values_only = TRUE), 1207.01)
  expect_equal(metar_visibility(x7, numeric_values_only = TRUE), 16093.44)
  expect_equal(metar_visibility(x8, numeric_values_only = TRUE), 9999)
  expect_equal(metar_visibility(x, numeric_values_only = TRUE), c(9999, 48280.32, 10000, 2816.35, 3621.02, 1207.01, 16093.44, 9999))
  expect_equal(metar_visibility(dx$metar, numeric_values_only = TRUE), c(9999, 48280.32, 10000, 2816.35, 3621.02, 1207.01, 16093.44, 9999))
})


test_that("Check visibility in miles", {
  expect_equal(metar_visibility(x1, metric = FALSE), "6.21")
  expect_equal(metar_visibility(x2, metric = FALSE), "30")
  expect_equal(metar_visibility(x3, metric = FALSE), "Ceiling And Visibility OK")
  expect_equal(metar_visibility(x4, metric = FALSE), "1.75")
  expect_equal(metar_visibility(x5, metric = FALSE), "2.25")
  expect_equal(metar_visibility(x6, metric = FALSE), "0.75")
  expect_equal(metar_visibility(x7, metric = FALSE), "10")
  expect_equal(metar_visibility(x8, metric = FALSE), "6.21")
  expect_equal(metar_visibility(x, metric = FALSE), c("6.21", "30", "Ceiling And Visibility OK", "1.75", "2.25", "0.75", "10", "6.21"))
  expect_equal(metar_visibility(dx$metar, metric = FALSE), c("6.21", "30", "Ceiling And Visibility OK", "1.75", "2.25", "0.75", "10", "6.21"))
})

test_that("Check visibility in miles, numeric values only", {
  expect_equal(metar_visibility(x1, metric = FALSE, numeric_values_only = TRUE), 6.21)
  expect_equal(metar_visibility(x2, metric = FALSE, numeric_values_only = TRUE), 30)
  expect_equal(metar_visibility(x3, metric = FALSE, numeric_values_only = TRUE), 6.21)
  expect_equal(metar_visibility(x4, metric = FALSE, numeric_values_only = TRUE), 1.75)
  expect_equal(metar_visibility(x5, metric = FALSE, numeric_values_only = TRUE), 2.25)
  expect_equal(metar_visibility(x6, metric = FALSE, numeric_values_only = TRUE), 0.75)
  expect_equal(metar_visibility(x7, metric = FALSE, numeric_values_only = TRUE), 10)
  expect_equal(metar_visibility(x8, metric = FALSE, numeric_values_only = TRUE), 6.21)
  expect_equal(metar_visibility(x, metric = FALSE, numeric_values_only = TRUE),
               c(6.21, 30, 6.21, 1.75, 2.25, 0.75, 10, 6.21))
  expect_equal(metar_visibility(dx$metar, metric = FALSE, numeric_values_only = TRUE),
               c(6.21, 30, 6.21, 1.75, 2.25, 0.75, 10, 6.21))
})

x9 <- "EPWA 281830Z 18009KT 140V200 99999 SCT037 03/M01 Q1008 NOSIG"
x10 <- "CYUL 281800Z 13008KT 30 SM BKN240 01/M06 A3005 RMK CI5 SLP180"
x11 <- "201711271930 METAR LEMD 271930Z 02002KT cAVOK 04/M03 Q1025 NOSIG= NOSIG="
x12 <- "KBLV 011657Z AUTO 25015G30KT 210V290 10/8SM R32L/1000FT FG BKN005 01/M01 A2984 RMK A02 SLP03"

test_that("Incorrect METAR reports", {
  expect_equal(is.na(metar_visibility(x9)), TRUE)
  expect_equal(is.na(metar_visibility(x10)), TRUE)
  expect_equal(is.na(metar_visibility(x11)), TRUE)
  expect_equal(is.na(metar_visibility(x9, numeric_values_only = TRUE)), TRUE)
  expect_equal(is.na(metar_visibility(x10, numeric_values_only = TRUE)), TRUE)
  expect_equal(is.na(metar_visibility(x11, numeric_values_only = TRUE)), TRUE)
  expect_equal(is.na(metar_visibility(x9, metric = FALSE)), TRUE)
  expect_equal(is.na(metar_visibility(x10, metric = FALSE)), TRUE)
  expect_equal(is.na(metar_visibility(x11, metric = FALSE)), TRUE)
  expect_equal(is.na(metar_visibility(x9, metric = FALSE, numeric_values_only = TRUE)), TRUE)
  expect_equal(is.na(metar_visibility(x10, metric = FALSE, numeric_values_only = TRUE)), TRUE)
  expect_equal(is.na(metar_visibility(x11, metric = FALSE, numeric_values_only = TRUE)), TRUE)

})

dx <- data.frame(metar = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12))

test_that("Correct and incorrect METAR reports", {
  expect_equal(metar_visibility(dx$metar, metric = TRUE), c("9999", "48280.32", "Ceiling And Visibility OK", "2816.35", "3621.02", "1207.01",
                                                            "16093.44", "9999", NA, NA, NA, NA))
  expect_equal(metar_visibility(dx$metar, metric = FALSE), c("6.21", "30", "Ceiling And Visibility OK", "1.75", "2.25", "0.75", "10", "6.21",
                                                            NA, NA, NA, NA))
  expect_equal(metar_visibility(dx$metar, metric = TRUE, numeric_values_only = TRUE),
               c(9999, 48280.32, 10000, 2816.35, 3621.02, 1207.01, 16093.44, 9999, NA, NA, NA, NA))
  expect_equal(metar_visibility(dx$metar, metric = FALSE, numeric_values_only = TRUE),
               c(6.21, 30, 6.21, 1.75, 2.25, 0.75, 10, 6.21, NA, NA, NA, NA))
})

dxt <- tibble::as_tibble(dx)

test_that("Check invalid input format", {
  expect_error(metar_visibility(dx))
  expect_error(metar_visibility(dxt))
})

