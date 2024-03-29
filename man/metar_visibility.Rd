% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metar_visibility.R
\name{metar_visibility}
\alias{metar_visibility}
\title{Get visibility information.}
\usage{
metar_visibility(x, metric = TRUE, numeric_only = FALSE)
}
\arguments{
\item{x}{character vector; a METAR weather report or reports.}

\item{metric}{For the default metric = TRUE returned distances are in meters. If it's FALSE, in miles.}

\item{numeric_only}{logical; if TRUE only a numeric value will be returned}
}
\value{
a numeric vector with visibility information, in meters or miles.
}
\description{
Extract and parse visibility information from METAR weather report.
}
\examples{
metar_visibility("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
metar_visibility("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
metar_visibility("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025")
metar_visibility("KBLV 011657Z AUTO 25015G30KT 210V290 3/8SM R32L/1000FT FG
BKN005 01/M01 A2984")

}
