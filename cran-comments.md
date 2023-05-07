## Test environments, status OK for:
* R CMD check --as-cran
* local Xubuntu 22.04 LTS, R 4.2.1
* win-builder (development and release)
* rhub macOS 10.13.6 High Sierra, R-release, CRAN's setup
* rhub Debian Linux, R-release, GCC 
* rhub Fedora Linux, R-devel, GCC
* rhub Ubuntu Linux 20.04.1 LTS, R-release, GCC

## R CMD check results
* 1 NOTE for the win-builder: Found the following (possibly) invalid URLs https://ourairports.com However the web page is online.

## Others

Possibly mis-spelled words in DESCRIPTION:  
  
  ASOS
  AWOS
  METAR
  Mesonet

Three mis-spelled words are the abbreviations:  

ASOS for Automated Surface Observing System  
AWOS for Automated Weather Observing System  
METAR for Meteorological Terminal Aviation Routine Weather Report  
  
And the meaning of the last one Mesonet according to
<https://en.wikipedia.org/wiki/Mesonet> is "in meteorology (and
climatology), a mesonet, portmanteau of mesoscale network, is a network
of (typically) automated weather and environmental monitoring stations
designed to observe mesoscale meteorological phenomena."  

This is a new version 0.4.0 release.


