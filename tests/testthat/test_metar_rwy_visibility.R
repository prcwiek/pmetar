context("METAR RWY visibility")

x1 <- "EBBR 040220Z VRB01KT 0150 R25L/1200N R02/P1500 07/06 Q1017"
x2 <- "EBBR 040220Z VRB01KT 0150 R25R/0600FT R02/P1500 FG BKN001 07/06 Q1017 NOSIG="
x3 <- "EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG"
x4 <- "CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D -SN M05/M08 A2992"
x5 <- "EBBR 040220Z VRB01KT 0150 R25L/1200N R25R/0600FT R02/P1500 FG BKN001 07/06 Q1017 NOSIG="
x6 <- "EDDF 220520Z 26003KT 0500 R25R/0400N R25C/P2000N R25L/P2000N R18/0650V1100N FZFG BKN001 BKN003 M08/M09 Q1015 NOSIG"


x <- c(x1, x2, x3, x4, x5, x6)

dx <- data.frame(metar = x)

test_that("Check runway visibility in meters, default sep = ';'", {
  expect_equal(metar_rwy_visibility(x1, metric = TRUE),
               "Runway visual range for runway R25L is 1200 meters with static trend; Runway visual range for runway R02 is greater than 1500 meters")
  expect_equal(metar_rwy_visibility(x2, metric = TRUE),
               "Runway visual range for runway R25R is 182.88 meters; Runway visual range for runway R02 is greater than 1500 meters")
  expect_equal(metar_rwy_visibility(x3, metric = TRUE), "")
  expect_equal(metar_rwy_visibility(x4, metric = TRUE), "Runway visual range for runway R36 is 1219.2 meters with downward trend")
  expect_equal(metar_rwy_visibility(x5, metric = TRUE),
               "Runway visual range for runway R25L is 1200 meters with static trend; Runway visual range for runway R25R is 182.88 meters; Runway visual range for runway R02 is greater than 1500 meters")
  expect_equal(metar_rwy_visibility(x6, metric = TRUE),
               "Runway visual range for runway R25R is 400 meters with static trend; Runway visual range for runway R25C is greater than 2000 meters; Runway visual range for runway R25L is greater than 2000 meters; Runway visual range for runway R18 is from 650 to 1100 meters")
  expect_equal(metar_rwy_visibility(x, metric = TRUE),
               c("Runway visual range for runway R25L is 1200 meters with static trend; Runway visual range for runway R02 is greater than 1500 meters",
                 "Runway visual range for runway R25R is 182.88 meters; Runway visual range for runway R02 is greater than 1500 meters",
                 "",
                 "Runway visual range for runway R36 is 1219.2 meters with downward trend",
                 "Runway visual range for runway R25L is 1200 meters with static trend; Runway visual range for runway R25R is 182.88 meters; Runway visual range for runway R02 is greater than 1500 meters",
                 "Runway visual range for runway R25R is 400 meters with static trend; Runway visual range for runway R25C is greater than 2000 meters; Runway visual range for runway R25L is greater than 2000 meters; Runway visual range for runway R18 is from 650 to 1100 meters"))
  expect_equal(metar_rwy_visibility(dx$metar, metric = TRUE),
               c("Runway visual range for runway R25L is 1200 meters with static trend; Runway visual range for runway R02 is greater than 1500 meters",
                 "Runway visual range for runway R25R is 182.88 meters; Runway visual range for runway R02 is greater than 1500 meters",
                 "",
                 "Runway visual range for runway R36 is 1219.2 meters with downward trend",
                 "Runway visual range for runway R25L is 1200 meters with static trend; Runway visual range for runway R25R is 182.88 meters; Runway visual range for runway R02 is greater than 1500 meters",
                 "Runway visual range for runway R25R is 400 meters with static trend; Runway visual range for runway R25C is greater than 2000 meters; Runway visual range for runway R25L is greater than 2000 meters; Runway visual range for runway R18 is from 650 to 1100 meters"))
})

test_that("Check runway visibility in meters, sep = ','", {
  expect_equal(metar_rwy_visibility(x1, metric = TRUE, sep = ","),
               "Runway visual range for runway R25L is 1200 meters with static trend, Runway visual range for runway R02 is greater than 1500 meters")
  expect_equal(metar_rwy_visibility(x2, metric = TRUE, sep = ","),
               "Runway visual range for runway R25R is 182.88 meters, Runway visual range for runway R02 is greater than 1500 meters")
  expect_equal(metar_rwy_visibility(x3, metric = TRUE, sep = ","), "")
  expect_equal(metar_rwy_visibility(x4, metric = TRUE, sep = ","), "Runway visual range for runway R36 is 1219.2 meters with downward trend")
  expect_equal(metar_rwy_visibility(x5, metric = TRUE, sep = ","),
               "Runway visual range for runway R25L is 1200 meters with static trend, Runway visual range for runway R25R is 182.88 meters, Runway visual range for runway R02 is greater than 1500 meters")
  expect_equal(metar_rwy_visibility(x6, metric = TRUE, sep = ","),
               "Runway visual range for runway R25R is 400 meters with static trend, Runway visual range for runway R25C is greater than 2000 meters, Runway visual range for runway R25L is greater than 2000 meters, Runway visual range for runway R18 is from 650 to 1100 meters")
  expect_equal(metar_rwy_visibility(x, metric = TRUE, sep = ","),
               c("Runway visual range for runway R25L is 1200 meters with static trend, Runway visual range for runway R02 is greater than 1500 meters",
                 "Runway visual range for runway R25R is 182.88 meters, Runway visual range for runway R02 is greater than 1500 meters",
                 "",
                 "Runway visual range for runway R36 is 1219.2 meters with downward trend",
                 "Runway visual range for runway R25L is 1200 meters with static trend, Runway visual range for runway R25R is 182.88 meters, Runway visual range for runway R02 is greater than 1500 meters",
                 "Runway visual range for runway R25R is 400 meters with static trend, Runway visual range for runway R25C is greater than 2000 meters, Runway visual range for runway R25L is greater than 2000 meters, Runway visual range for runway R18 is from 650 to 1100 meters"))
  expect_equal(metar_rwy_visibility(dx$metar, metric = TRUE, sep = ","),
               c("Runway visual range for runway R25L is 1200 meters with static trend, Runway visual range for runway R02 is greater than 1500 meters",
                 "Runway visual range for runway R25R is 182.88 meters, Runway visual range for runway R02 is greater than 1500 meters",
                 "",
                 "Runway visual range for runway R36 is 1219.2 meters with downward trend",
                 "Runway visual range for runway R25L is 1200 meters with static trend, Runway visual range for runway R25R is 182.88 meters, Runway visual range for runway R02 is greater than 1500 meters",
                 "Runway visual range for runway R25R is 400 meters with static trend, Runway visual range for runway R25C is greater than 2000 meters, Runway visual range for runway R25L is greater than 2000 meters, Runway visual range for runway R18 is from 650 to 1100 meters"))
})

test_that("Check runway visibility in feet, default sep = ';", {
  expect_equal(metar_rwy_visibility(x1, metric = FALSE),
               "Runway visual range for runway R25L is 3937.01 ft with static trend; Runway visual range for runway R02 is greater than 4921.26 ft")
  expect_equal(metar_rwy_visibility(x2, metric = FALSE),
               "Runway visual range for runway R25R is 600 ft; Runway visual range for runway R02 is greater than 4921.26 ft")
  expect_equal(metar_rwy_visibility(x3, metric = FALSE), "")
  expect_equal(metar_rwy_visibility(x4, metric = FALSE), "Runway visual range for runway R36 is 4000 ft with downward trend")
  expect_equal(metar_rwy_visibility(x5, metric = FALSE),
               "Runway visual range for runway R25L is 3937.01 ft with static trend; Runway visual range for runway R25R is 600 ft; Runway visual range for runway R02 is greater than 4921.26 ft")
  expect_equal(metar_rwy_visibility(x6, metric = FALSE),
               "Runway visual range for runway R25R is 1312.34 ft with static trend; Runway visual range for runway R25C is greater than 6561.68 ft; Runway visual range for runway R25L is greater than 6561.68 ft; Runway visual range for runway R18 is from 2132.55 to 3608.92 ft")
  expect_equal(metar_rwy_visibility(x, metric = FALSE),
               c("Runway visual range for runway R25L is 3937.01 ft with static trend; Runway visual range for runway R02 is greater than 4921.26 ft",
                 "Runway visual range for runway R25R is 600 ft; Runway visual range for runway R02 is greater than 4921.26 ft",
                 "",
                 "Runway visual range for runway R36 is 4000 ft with downward trend",
                 "Runway visual range for runway R25L is 3937.01 ft with static trend; Runway visual range for runway R25R is 600 ft; Runway visual range for runway R02 is greater than 4921.26 ft",
                 "Runway visual range for runway R25R is 1312.34 ft with static trend; Runway visual range for runway R25C is greater than 6561.68 ft; Runway visual range for runway R25L is greater than 6561.68 ft; Runway visual range for runway R18 is from 2132.55 to 3608.92 ft"))
  expect_equal(metar_rwy_visibility(dx$metar, metric = FALSE),
               c("Runway visual range for runway R25L is 3937.01 ft with static trend; Runway visual range for runway R02 is greater than 4921.26 ft",
                 "Runway visual range for runway R25R is 600 ft; Runway visual range for runway R02 is greater than 4921.26 ft",
                 "",
                 "Runway visual range for runway R36 is 4000 ft with downward trend",
                 "Runway visual range for runway R25L is 3937.01 ft with static trend; Runway visual range for runway R25R is 600 ft; Runway visual range for runway R02 is greater than 4921.26 ft",
                 "Runway visual range for runway R25R is 1312.34 ft with static trend; Runway visual range for runway R25C is greater than 6561.68 ft; Runway visual range for runway R25L is greater than 6561.68 ft; Runway visual range for runway R18 is from 2132.55 to 3608.92 ft"))
})

x6 <- "EBBR 040220Z VRB01KT 0150 R25L//1200N 07/06 Q1017"
x7 <- "EBBR 040220Z VRB01KT 0150 R25L/X1200N R02222/1500 07/06 Q1017"
x8 <- "CYWG 172000Z 30015G25KT 3/4SM R36LL/4000FT/D -SN M05/M08 A2992"

test_that("Incorrect METAR reports", {
  expect_equal(metar_rwy_visibility(x6), "")
  expect_equal(metar_rwy_visibility(x7), "")
  expect_equal(metar_rwy_visibility(x8), "")
})

dx <- data.frame(metar = c(x1, x2, x3, x4, x5, x6, x7, x8))

test_that("Correct and incorrect METAR reports",{
  expect_equal(metar_rwy_visibility(dx$metar, metric = TRUE),
               c("Runway visual range for runway R25L is 1200 meters with static trend; Runway visual range for runway R02 is greater than 1500 meters",
                 "Runway visual range for runway R25R is 182.88 meters; Runway visual range for runway R02 is greater than 1500 meters",
                 "",
                 "Runway visual range for runway R36 is 1219.2 meters with downward trend",
                 "Runway visual range for runway R25L is 1200 meters with static trend; Runway visual range for runway R25R is 182.88 meters; Runway visual range for runway R02 is greater than 1500 meters",
                 "", "", ""))
  expect_equal(metar_rwy_visibility(dx$metar, metric = FALSE),
               c("Runway visual range for runway R25L is 3937.01 ft with static trend; Runway visual range for runway R02 is greater than 4921.26 ft",
                 "Runway visual range for runway R25R is 600 ft; Runway visual range for runway R02 is greater than 4921.26 ft",
                 "",
                 "Runway visual range for runway R36 is 4000 ft with downward trend",
                 "Runway visual range for runway R25L is 3937.01 ft with static trend; Runway visual range for runway R25R is 600 ft; Runway visual range for runway R02 is greater than 4921.26 ft",
                 "", "", ""))
})

dxt <- tibble::as_tibble(dx)

test_that("Check invalid input format", {
  expect_error(metar_visibility(dx))
  expect_error(metar_visibility(dxt))
})

