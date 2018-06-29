<?php

$path = $argv[1];

print_r(array(
	'path' => $path,
	'list_id' => get_esp_list_id_from_path($path),
));

function get_esp_list_id_from_path($path) {

	$path = stripcslashes($path);
	$parameters = array();

	if (preg_match("/\?/im", $path)) {
		$parts = explode("?", $path);
		$path = $parts[1];
	}


	parse_str($path, $parameters);
	print_r($parameters);

	if (isset($parameters['lid'])) {
		return $parameters['lid'];
	} else {
		return 0;
	}
}
