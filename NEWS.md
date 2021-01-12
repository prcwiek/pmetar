pmetar version 0.3.0
===========================

### BUG FIXES

* metar_cloud_coverage() fixed failures with R old releases
* metar_wx_codes() fixed failures with R old releases
* metar_speed() fixed problems with zero wind speed value when wind direction is variable (VRB)
* metar_cloud_coverage() fixed issue with BKN///
* metar_pressure fixed issue with parsing pressure at the end of METAR reports

### NEW FEATURES

* Added option altimeter in metar_decode for selecting pressure value, hPa or mmHg
* Added function metar_rwy_visibility for decoding runway(s) visibility

pmetar version 0.2.7.0
==============

### NEW FEATURES

* Released to CRAN
