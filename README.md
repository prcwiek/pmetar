
# pmetar
Package allows to download current and historical METAR weather reports and decode basic parameters.

<h4><b>METAR weather reports source web pages</b></h4>
<p>The function metar_get scraps data from two web pages:<br>
<ol>
<li>For current reports: Aviation Weather Center https://www.aviationweather.gov/metar</li><br>
<li>For historical reports: <br>
<ul>
<li>Iowa Environmental Mesonet web page of Iowa State University
ASOS-AWOS-METAR http://mesonet.agron.iastate.edu/AWOS/</li>
<li>Weather Information Service http://www.ogimet.com/ developed by Guillermo Ballester Valor</li>
</ul></ol>
Please take into consideration that the http://www.ogimet.com/ can block too frequent requests for data due to the server overload. <br>
<br><h4><b>Locations of METAR stations</b></h4>
<p>Information about the locations of the METAR stations / ariports were taken from two sources:<br>
<ol>
<li> The first choice is the file from https://ourairports.com/data/ created by David Megginson.</li>
<li> If information can't be found, the second source is searched, the ADDS Station Table prepared by Greg Thompson NCAR/RAP. The file is available on the web page of Aviation Weather Center https://www.aviationweather.gov/docs/metar/stations.txt 
from NOAA National Weather Service https://www.weather.gov/</li>
</ol>
The both above data sources are in the public domain.
<br><br>
<h4><b>Package pmetar is for extracting and parsing information, from current or historical METAR reports, only.
<br><br>
Don't use it for flight planning or navigation.</b></h4>
