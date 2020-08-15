context("METAR iata icao")

x1 <- "EPWA"
x2 <- "CYUL"
x3 <- "MAD"
x4 <- "WAW"
x5 <- "jfk"
x6 <- "kewr"

x <- c(x1, x2, x3, x4, x5, x6)

dx <- data.frame(metar = x)

test_that("Check if output is character", {
  expect_equal(is.character(metar_iata_icao(x1)), TRUE)
  expect_equal(is.character(metar_iata_icao(x2)), TRUE)
  expect_equal(is.character(metar_iata_icao(x3)), TRUE)
  expect_equal(is.character(metar_iata_icao(x4)), TRUE)
  expect_equal(is.character(metar_iata_icao(x5)), TRUE)
  expect_equal(is.character(metar_iata_icao(x6)), TRUE)
  expect_equal(is.character(metar_iata_icao(x)), TRUE)
  expect_equal(is.character(metar_iata_icao(dx$metar)), TRUE)
})

test_that("Check correct output", {
  expect_equal(metar_iata_icao(x1), "WAW")
  expect_equal(metar_iata_icao(x2), "YUL")
  expect_equal(metar_iata_icao(x3), "LEMD")
  expect_equal(metar_iata_icao(x4), "EPWA")
  expect_equal(metar_iata_icao(x5), "KJFK")
  expect_equal(metar_iata_icao(x6), "EWR")
  expect_equal(metar_iata_icao(x), c("WAW", "YUL", "LEMD", "EPWA", "KJFK", "EWR"))
  expect_equal(metar_iata_icao(dx$metar), c("WAW", "YUL", "LEMD", "EPWA", "KJFK", "EWR"))
})

x1 <- "aa"
x2 <- "AcdEE"
x3 <- "a "
x4 <- "AAAA"
x5 <- "EPWAa"
x6 <- 123
x7 <- "ZYTH"

x <- c(x1, x2, x3, x4, x5, x6, x7)

dx <- data.frame(metar = x)

test_that("Check incorrect inputs", {
  expect_equal(metar_iata_icao(x1), "Incorrect ICAO or IATA airport code!")
  expect_equal(metar_iata_icao(x2), "Incorrect ICAO or IATA airport code!")
  expect_equal(metar_iata_icao(x3), "Incorrect ICAO or IATA airport code!")
  expect_equal(metar_iata_icao(x4), "Incorrect ICAO or IATA airport code!")
  expect_equal(metar_iata_icao(x5), "Incorrect ICAO or IATA airport code!")
  expect_equal(metar_iata_icao(x6), "Incorrect ICAO or IATA airport code!")
  expect_equal(metar_iata_icao(x7), "Not found!")
  expect_equal(metar_iata_icao(x), c(rep("Incorrect ICAO or IATA airport code!", 6), "Not found!"))
  expect_equal(metar_iata_icao(dx$metar), c(rep("Incorrect ICAO or IATA airport code!", 6), "Not found!"))
})

