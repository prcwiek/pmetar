context("METAR WX codes")

x1 <- "EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG"
x2 <- "CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180"
x3 <- "202001190045 METAR KEWR 190045Z AUTO 19008KT 4SM -RA -PL BR FEW007 BKN011 OVC016 01/M01 A2995 RMK P0005 T00101010 MADISHF"
x4 <- "202001041051 METAR KEWR 041051Z 07003KT 2SM BR FEW006 OVC014 08/08 A2971 RMK TWR VIS 2 1/2 RAE46 SLP058 P0003 T00830078"
x5 <- "201905121244 SPECI KDCA 121244Z 05010KT 2 1/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T11391122"
x6 <- "CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D -SN BLSN BKN008 OVC040 M05/M08 A2992 REFZRA WS RWY36 RMK SF5NS3 SLP134"
x7 <- "METAR KJFK 282355Z AUTO 13009KT 10SM -RA SCT028 SCT035 BKN079 23/20 A2972 RMK T02300200  LTG DSNT SE-SW! MADISHF"
x8 <- "201807141200 METAR EPWA 141200Z 30011G22KT 270V340 9999 -SHRA SCT007 BKN015CB 18/17 Q1011 RESHRA TEMPO BKN007"
x9 <- "202002022205 METAR KEWR 022205Z AUTO 24008KT 6SM -RA -SN BR SCT006 BKN014 OVC024 02/01 A2954 RMK T00200010 MADISHF"
x10 <- "202002130144 METAR KEWR 130144Z 17006KT 10SM -RAPL SCT060 OVC080 06/M01 A3010 RMK AO2 RAB39PLB44 P0000 T00611006"

x <- c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)

dx <- data.frame(metar = x)

test_that("Check WX codes, default sep = ';'" , {
  expect_equal(metar_wx_codes(x1), "")
  expect_equal(metar_wx_codes(x2), "")
  expect_equal(metar_wx_codes(x3), "Light intensity: Rain; Light intensity: Ice Pellets; Mist (French: Brume)")
  expect_equal(metar_wx_codes(x4), "Mist (French: Brume)")
  expect_equal(metar_wx_codes(x5), "Light intensity: Rain; Mist (French: Brume)")
  expect_equal(metar_wx_codes(x6), "Light intensity: Snow; Blowing Snow; Recent: Freezing Rain")
  expect_equal(metar_wx_codes(x7), "Light intensity: Rain")
  expect_equal(metar_wx_codes(x8), "Light intensity: Showers Rain; Recent: Showers Rain")
  expect_equal(metar_wx_codes(x9), "Light intensity: Rain; Light intensity: Snow; Mist (French: Brume)")
  expect_equal(metar_wx_codes(x10), "Light intensity: Rain Ice Pellets")
  expect_equal(metar_wx_codes(x), c("", "",
                                    "Light intensity: Rain; Light intensity: Ice Pellets; Mist (French: Brume)",
                                    "Mist (French: Brume)",
                                    "Light intensity: Rain; Mist (French: Brume)",
                                    "Light intensity: Snow; Blowing Snow; Recent: Freezing Rain",
                                    "Light intensity: Rain",
                                    "Light intensity: Showers Rain; Recent: Showers Rain",
                                    "Light intensity: Rain; Light intensity: Snow; Mist (French: Brume)",
                                    "Light intensity: Rain Ice Pellets"))
  expect_equal(metar_wx_codes(dx$metar), c("", "",
                                           "Light intensity: Rain; Light intensity: Ice Pellets; Mist (French: Brume)",
                                           "Mist (French: Brume)",
                                           "Light intensity: Rain; Mist (French: Brume)",
                                           "Light intensity: Snow; Blowing Snow; Recent: Freezing Rain",
                                           "Light intensity: Rain",
                                           "Light intensity: Showers Rain; Recent: Showers Rain",
                                           "Light intensity: Rain; Light intensity: Snow; Mist (French: Brume)",
                                           "Light intensity: Rain Ice Pellets"))
})

test_that("Check WX codes, sep = ','" , {
  expect_equal(metar_wx_codes(x1, sep = ","), "")
  expect_equal(metar_wx_codes(x2, sep = ","), "")
  expect_equal(metar_wx_codes(x3, sep = ","), "Light intensity: Rain, Light intensity: Ice Pellets, Mist (French: Brume)")
  expect_equal(metar_wx_codes(x4, sep = ","), "Mist (French: Brume)")
  expect_equal(metar_wx_codes(x5, sep = ","), "Light intensity: Rain, Mist (French: Brume)")
  expect_equal(metar_wx_codes(x6, sep = ","), "Light intensity: Snow, Blowing Snow, Recent: Freezing Rain")
  expect_equal(metar_wx_codes(x7, sep = ","), "Light intensity: Rain")
  expect_equal(metar_wx_codes(x8, sep = ","), "Light intensity: Showers Rain, Recent: Showers Rain")
  expect_equal(metar_wx_codes(x9, sep = ","), "Light intensity: Rain, Light intensity: Snow, Mist (French: Brume)")
  expect_equal(metar_wx_codes(x10, sep = ","), "Light intensity: Rain Ice Pellets")
  expect_equal(metar_wx_codes(x, sep = ","), c("", "",
                                    "Light intensity: Rain, Light intensity: Ice Pellets, Mist (French: Brume)",
                                    "Mist (French: Brume)",
                                    "Light intensity: Rain, Mist (French: Brume)",
                                    "Light intensity: Snow, Blowing Snow, Recent: Freezing Rain",
                                    "Light intensity: Rain",
                                    "Light intensity: Showers Rain, Recent: Showers Rain",
                                    "Light intensity: Rain, Light intensity: Snow, Mist (French: Brume)",
                                    "Light intensity: Rain Ice Pellets"))
  expect_equal(metar_wx_codes(dx$metar, sep = ","), c("", "",
                                           "Light intensity: Rain, Light intensity: Ice Pellets, Mist (French: Brume)",
                                           "Mist (French: Brume)",
                                           "Light intensity: Rain, Mist (French: Brume)",
                                           "Light intensity: Snow, Blowing Snow, Recent: Freezing Rain",
                                           "Light intensity: Rain",
                                           "Light intensity: Showers Rain, Recent: Showers Rain",
                                           "Light intensity: Rain, Light intensity: Snow, Mist (French: Brume)",
                                           "Light intensity: Rain Ice Pellets"))
})

x11 <- "202001190045 METAR KEWR 190045Z AUTO 19008KT 4SM /RA PPLBR FEW007 BKN011 OVC016 01/M01 A2995 RMK P0005 T00101010 MADISHF"
x12 <- "202001041051 METAR KEWR 041051Z 07003KT 2SM REEBR FEW006 OVC014 08/08 A2971 RMK TWR VIS 2 1/2 RAE46 SLP058 P0003 T00830078"
x13 <- "201905121244 SPECI KDCA 121244Z 05010KT 2 1/4SM R01/6000VP6000FT AR RB OVC007 14/12 A2978 RMK AO2 P0002 T11391122"


test_that("Incorrect METAR reports", {
  expect_equal(metar_wx_codes(x11), "")
  expect_equal(metar_wx_codes(x12), "")
  expect_equal(metar_wx_codes(x13), "")
})

dx <- data.frame(metar = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13))

test_that("Correct and incorrect METAR reports", {
  expect_equal(metar_wx_codes(dx$metar), c("", "",
                                           "Light intensity: Rain; Light intensity: Ice Pellets; Mist (French: Brume)",
                                           "Mist (French: Brume)",
                                           "Light intensity: Rain; Mist (French: Brume)",
                                           "Light intensity: Snow; Blowing Snow; Recent: Freezing Rain",
                                           "Light intensity: Rain",
                                           "Light intensity: Showers Rain; Recent: Showers Rain",
                                           "Light intensity: Rain; Light intensity: Snow; Mist (French: Brume)",
                                           "Light intensity: Rain Ice Pellets",
                                           "", "", ""))

})



dxt <- tibble::as_tibble(dx)

test_that("Check invalid input format", {
  expect_error(metar_wx_codes(dx))
  expect_error(metar_wx_codes(dxt))
})


