pmetar version 0.4.0 
====================

### BUG FIXES

* Fixed problems with downloading historical data from the Ogimet server
* Fixed several bugs with METAR reports decoding

### NEW FEATURES

* Added parameter numeric_only to metar_decode()
* Added parameter check to metar_decode()
* Added parameter sep to metar_decode()
* Added new function metar_is_correct()
* Added new function metar_print()
* Update of metar_decode() for better handling errors 

### OTHERS

* Update of both airports lists
* Update of vignettes
* Improving how the packages which use Internet resources should fail gracefully with 
an informative message.

pmetar version 0.3.3
====================

### BUG FIXES

* metar_decode() fixed fails with METAR COR

pmetar version 0.3.2
====================

### BUG FIXES

* Packages which use Internet resources should fail gracefully with an informative message.

pmetar version 0.3.1
====================

### BUG FIXES

* metar_get() fixed failures with Rcurl, OpenSSL and TLS on Windows

pmetar version 0.3.0
====================

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
======================

### NEW FEATURES

* Released to CRAN
