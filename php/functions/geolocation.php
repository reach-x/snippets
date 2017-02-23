<?php

$geo_array = unserialize(file_get_contents('http://www.geoplugin.net/php.gp?ip=69.89.67.34'));

print("Country: " . $geo_array['geoplugin_countryName'] . "\n");
print("State: " . $geo_array['geoplugin_regionName'] . "\n");


