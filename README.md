
# pmetar
Package allows to download current and historical METAR weather reports and decode basic parameters.

<h4><b>METAR weather reports source web pages</b></h4>
The function metar_get scraps data from two web pages:<br><br>
<ol>
<li>For current reports: Aviation Weather Center https://www.aviationweather.gov/metar</li>
<li>For historical reports: 
<ul>
<li>Iowa Environmental Mesonet web page of Iowa State University
ASOS-AWOS-METAR http://mesonet.agron.iastate.edu/AWOS/</li>
<li>Weather Information Service http://www.ogimet.com/ developed by Guillermo Ballester Valor</li>
</ul></ol>
Please take into consideration that the http://www.ogimet.com/ can block too frequent requests for data due to the server overload. <br>
<br><h4><b>Locations of METAR stations</b></h4>
Information about the locations of the METAR stations / ariports were taken from the file http://ourairports.com/data/airports.csv available on the web page http://ourairports.com 
