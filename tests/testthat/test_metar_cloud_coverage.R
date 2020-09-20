context("METAR cloud coverage")

x1 <- "EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG"
x2 <- "CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180"
x3 <- "201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG="
x4 <- "201905121244 METAR KDCA 121244Z 05010KT 1 3/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T01390122"
x5 <- "201905121244 SPECI KDCA 121244Z 05010KT 2 1/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T01390122"
x6 <- "CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D -SN BLSN BKN008 OVC040 M05/M08 A2992 REFZRA WS RWY36 RMK SF5NS3 SLP134"
x7 <- "METAR KJFK 282355Z AUTO 13009KT 10SM -RA SCT028 SCT035 BKN079 23/20 A2972 RMK T02300200  LTG DSNT SE-SW! MADISHF"
x8 <- "RJTT 192000Z 36009KT 9999 FEW025 BKN/// 23/19 Q1013 NOSIG RMK 1SC025 A2993"

x <- c(x1, x2, x3, x4, x5, x6, x7, x8)

dx <- data.frame(metar = x)

test_that("Check cloud coverage output", {
  expect_equal(metar_cloud_coverage("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG"),
               "Scattered (3-4 oktas) at 3700 ft (1127.76 m)")
  expect_equal(metar_cloud_coverage("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180"),
               "Broken (5-7 oktas) at 24000 ft (7315.2 m)")
  expect_equal(metar_cloud_coverage("METAR CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180"),
               "Broken (5-7 oktas) at 24000 ft (7315.2 m)")
  expect_equal(metar_cloud_coverage("SPECI CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180"),
               "Broken (5-7 oktas) at 24000 ft (7315.2 m)")
  expect_equal(metar_cloud_coverage(x7),
               "Scattered (3-4 oktas) at 2800, 3500 ft (853.44, 1066.8 m), Broken (5-7 oktas) at 7900 ft (2407.92 m)")
  expect_equal(metar_cloud_coverage(x8),
               "Few (1-2 oktas) at 2500 ft (762 m), Broken clouds at NaN  ft ( m)")
  expect_equal(metar_cloud_coverage("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG="), "")
  expect_equal(metar_cloud_coverage("201711271930 SPECI LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG="), "")
  expect_is(metar_cloud_coverage(x), "character")
  expect_is(metar_cloud_coverage(dx$metar), "character")
})

x8 <- "SPECI CYUL 281800Z 13008KT 30SM BKN24 01/M06 A3005 RMK CI5 SLP180"
x9 <- "METAR CYUL 281800Z 13008KT 30SM BKN24 BKN300 01/M06 A3005 RMK CI5 SLP180"
x10 <- "SPECI CYUL 281800Z 13008KT 30SM BKNBKN 01/M06 A3005 RMK CI5 SLP180"

test_that("Incorrect METAR reports", {
  expect_equal(metar_cloud_coverage(x8), "")
  expect_equal(metar_cloud_coverage(x9),
               "Broken (5-7 oktas) at 30000 ft (9144 m)")
  expect_equal(metar_cloud_coverage(x10), "")
})

dx <- data.frame(metar = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))

test_that("Correct and incorrect METAR reports", {
  expect_is(metar_cloud_coverage(dx$metar), "character")
  expect_equal(metar_cloud_coverage(dx$metar)[2],  "Broken (5-7 oktas) at 24000 ft (7315.2 m)")
  expect_equal(metar_cloud_coverage(dx$metar)[10],  "")
})

dxt <- tibble::as_tibble(dx)

test_that("Check invalid input format", {
  expect_error(metar_cloud_coverage(dx))
  expect_error(metar_cloud_coverage(dxt))
})
