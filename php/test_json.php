<?php

$json = <<<EOJ
[{
	"PropertyId": "555",
	"FloorplanId": "555",
	"FloorplanName": "Studio",
	"Beds": "0",
	"Baths": "1.00",
	"AvailabilityURL": "",
	"UnitTypeMapping": ".058500"
}, {
	"PropertyId": "666",
	"FloorplanId": "666",
	"FloorplanName": "Studio",
	"Beds": "0",
	"Baths": "1.00",
	"AvailabilityURL": "",
	"UnitTypeMapping": ".058500"
}]
EOJ;

$data = json_decode($json, TRUE); //TRUE makes json_decode return an array

//printf("error: %s\n",json_last_error_msg());
//print_r(array('$data'=>$data));

$wanted_keys = array(
	"FloorplanName" => "",
	"Beds" => "",
	"Baths" => "",
);
$new_array = array_filter($data, function($datum) {

});

print_r($new_array);

