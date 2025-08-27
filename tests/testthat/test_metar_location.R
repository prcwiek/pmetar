context("METAR location")

x1 <- "EPWA"
x2 <- "CYUL"
x3 <- "MAD"
x4 <- "WAW"
x5 <- "jfk"
x6 <- "kewr"
x7 <- "NCRK"
x8 <- "LLL"

x <- c(x1, x2, x3, x4, x5, x6, x7, x8)

dx <- data.frame(metar = x)

test_that("Check if output is a data frame", {
  expect_equal(is.data.frame(metar_location(x1)), TRUE)
  expect_equal(is.data.frame(metar_location(x2)), TRUE)
  expect_equal(is.data.frame(metar_location(x3)), TRUE)
  expect_equal(is.data.frame(metar_location(x4)), TRUE)
  expect_equal(is.data.frame(metar_location(x5)), TRUE)
  expect_equal(is.data.frame(metar_location(x6)), TRUE)
  expect_equal(is.data.frame(metar_location(x7)), TRUE)
  expect_equal(is.data.frame(metar_location(x8)), TRUE)
  expect_equal(is.data.frame(metar_location(x)), TRUE)
  expect_equal(is.data.frame(metar_location(dx$metar)), TRUE)
})

# define output values
x1_output <- tibble::tibble(ICAO_Code = "EPWA", IATA_Code = "WAW",
                    Airport_Name = "Warsaw Chopin Airport",
                    Longitude = 20.9671, Latitude = 52.1657, Elevation = 110.3376,
                    Source = "http://ourairports.com/data/airports.csv")
x2_output <- tibble::tibble(ICAO_Code = "CYUL", IATA_Code = "YUL",
                    Airport_Name = "Montreal / Pierre Elliott Trudeau International Airport",
                    Longitude = -73.74229, Latitude = 45.46784, Elevation = 35.9664,
                    Source = "http://ourairports.com/data/airports.csv")
x3_output <- tibble::tibble(ICAO_Code = "LEMD", IATA_Code = "MAD",
                    Airport_Name = "Adolfo Suarez Madrid-Barajas Airport",
                    Longitude = -3.56264, Latitude = 40.47193, Elevation = 608.9904,
                    Source = "http://ourairports.com/data/airports.csv")
x4_output <- tibble::tibble(ICAO_Code = "EPWA", IATA_Code = "WAW",
                    Airport_Name = "Warsaw Chopin Airport",
                    Longitude = 20.9671, Latitude = 52.1657, Elevation = 110.3376,
                    Source = "http://ourairports.com/data/airports.csv")
x5_output <- tibble::tibble(ICAO_Code = "KJFK", IATA_Code = "JFK",
                    Airport_Name = "John F Kennedy International Airport",
                    Longitude = -73.77932, Latitude = 40.63945, Elevation = 3.9624,
                    Source = "http://ourairports.com/data/airports.csv")
x6_output <- tibble::tibble(ICAO_Code = "KEWR", IATA_Code = "EWR",
                    Airport_Name = "Newark Liberty International Airport",
                    Longitude = -74.1687, Latitude = 40.6925, Elevation = 5.4864,
                    Source = "http://ourairports.com/data/airports.csv")
x7_output <- tibble::tibble(ICAO_Code = "NCRK", IATA_Code = "Not found!",
                    Airport_Name = "RAKAHANGA ATOLL",
                    Longitude = -161.1, Latitude = -10.02, Elevation = 7,
                    Source = "www.aviationweather.gov/docs/metar/stations.txt")
x8_output <- tibble::tibble(ICAO_Code = "Not found!", IATA_Code = "Not found!",
                            Airport_Name = NA,
                            Longitude = NA, Latitude = NA, Elevation = NA,
                            Source = "Not found in pmetar sources!")

x_output <- rbind(x1_output, x2_output, x3_output,
                  x4_output, x5_output, x6_output,
                  x7_output, x8_output)

test_that("Check correct output", {
  expect_equal(metar_location(x1), x1_output)
  expect_equal(metar_location(x2), x2_output)
  expect_equal(metar_location(x3), x3_output)
  expect_equal(metar_location(x4), x4_output)
  expect_equal(metar_location(x5), x5_output)
  expect_equal(metar_location(x6), x6_output)
  expect_equal(metar_location(x7), x7_output)
  expect_equal(metar_location(x8), x8_output)
  expect_equal(metar_location(x), x_output)
  expect_equal(metar_location(dx$metar), x_output)
})

