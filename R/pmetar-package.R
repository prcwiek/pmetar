#' @description  Processing METAR Weather Reports
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' METAR WX weather conditions codes
#'
#' A dataset containing the explanations for METAR WX
#' weather conditions codes. The variables are as follows:
#'
#' \itemize{
#'   \item Type; type of the codes
#'   \item Abbreviation; the codes which are included in METAR reports
#'   \item Meaning; description of the codes
#' }
#'
#' @docType data
#' @keywords datasets
#' @name metarWXcodes
#' @references [https://en.wikipedia.org/wiki/METAR#METAR_WX_codes](https://en.wikipedia.org/wiki/METAR#METAR_WX_codes)
#' @format A data frame with 39 rows and 3 variables
NULL

#' Main list of airports
#'
#'#' A dataset containing the list of airports
#'
#' From <https://ourairports.com>
#' A data set is in the public domain according to <https://ourairports.com/data/>
#'
#' \itemize{
#'   \item id; identification number
#'   \item ident; airport ICAO code
#'   \item type; airport type
#'   \item name; airport name
#'   \item latitude_deg; geographical latitude
#'   \item longitude_deg; geographical longitude
#'   \item elevation_ft; airport elevation in feet
#'   \item elevation_m; airport elevation in meters
#'   \item iso_country; ISO country code
#'   \item iso_region; ISO region code
#'   \item municipality;
#'   \item iata_code; airport IATA code
#' }
#'
#' @docType data
#' @keywords datasets
#' @name ourairports
#' @author David Megginson
#' @references [https://ourairports.com/data/](https://ourairports.com/data/)
#' @format A data frame with 28935 rows and 12 variables
NULL

#' Secondary airport list
#'
#' A character vector containing the list of airports.
#'
#' From <https://www.aviationweather.gov/>
#' A data set is in the public domain according to <https://www.weather.gov/disclaimer>
#'
#' @docType data
#' @keywords datasets
#' @name mst
#' @author Greg Thompson from National Weather Service NCAR/RAP, NOAA National Weather Service
#' @references [https://weather.ral.ucar.edu/surface/stations.txt](https://weather.ral.ucar.edu/surface/stations.txt),
#' @format A character vector with the length of 10113 items
NULL
