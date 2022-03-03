
# pmetar

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN](https://www.r-pkg.org/badges/version/pmetar)](https://cran.r-project.org/package=pmetar)
[![cran
checks](https://cranchecks.info/badges/worst/pmetar)](https://cranchecks.info/pkgs/pmetar)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![monthly](https://cranlogs.r-pkg.org/badges/pmetar)](https://www.rpackages.io/package/pmetar)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/pmetar)](https://www.rpackages.io/package/pmetar)
<!-- badges: end -->

Package allows to download current and historical METAR weather reports
and decode basic parameters.

### METAR weather reports source web pages

The main functions scrap data from the below web pages:

1.  For current reports, the function *metar_get*:

-   Aviation Weather Center <https://www.aviationweather.gov/metar>
    </li>
    <br>

2.  For historical reports, the function *metar_get_historical*:

-   Iowa Environmental Mesonet web page of Iowa State University
    ASOS-AWOS-METAR <http://mesonet.agron.iastate.edu/AWOS/>
    </li>
-   Weather Information Service <http://www.ogimet.com/> developed by
    Guillermo Ballester Valor

Please take into consideration that the <http://www.ogimet.com/> can
block too frequent requests for data due to the server overload. <br>

### Locations of METAR stations

Information about the locations of the METAR stations / ariports were
taken from two sources:

1.  The first choice is the file from <https://ourairports.com/data/>
    created by David Megginson.
2.  If information can’t be found, the second source is searched, the
    ADDS Station Table prepared by Greg Thompson NCAR/RAP. The file is
    available on the web page of Aviation Weather Center
    <https://www.aviationweather.gov/docs/metar/stations.txt> from NOAA
    National Weather Service <https://www.weather.gov/>

The both above data sources are in the public domain.

#### Package pmetar is for extracting and parsing information, from current or historical METAR reports, only.

#### Don’t use it for flight planning or navigation!

### Installation

CRAN version

``` r
install.packages("pmetar")
```

Get the development version 0.3.2.01 from github:

``` r
# install.packages("devtools")
devtools::install_github("prcwiek/pmetar")
```

``` r
library('pmetar')
```

## Contributors

-   David Megginson, Author of data set with airports list
    <https://ourairports.com/data/>
-   Greg Thompson, Author of data set with airports list
    <https://www.aviationweather.gov/docs/metar/stations.txt>
