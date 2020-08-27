## Test environments
* local Xubuntu 20.04 LTS, R 4.0.2
* win-builder (devel and release)
* rhub macOS 10.13.6 High Sierra, R-release, CRAN's setup
* rhub Debian Linux, R-release, GCC
* rhub Fedora Linux, R-devel, GCC
* rhub Oracle Solaris 10, x86, 32 bit, R-release

## R CMD check results
* win-builder (devel and release)
There was 1 NOTE:  
  
Possibly mis-spelled words in DESCRIPTION:  
  
  ASOS (17:3)  
  AWOS (17:8)  
  METAR (3:19, 12:64, 17:13)  
  Mesonet (16:22)  

Three mis-spelled words are the abbreviations:  

ASOS for Automated Surface Observing System  
AWOS for Automated Weather Observing System  
METAR for Meteorological Terminal Aviation Routine Weather Report  
  
And the meaning of the last one Mesonet according to
<https://en.wikipedia.org/wiki/Mesonet> is "in meteorology (and
climatology), a mesonet, portmanteau of mesoscale network, is a network
of (typically) automated weather and environmental monitoring stations
designed to observe mesoscale meteorological phenomena."  
  
This is a new release, after the corrections according to CRAN comments.
