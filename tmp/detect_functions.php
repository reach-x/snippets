<?php
function locateIp($ip) {

	 return array(
		'ip' => $ip,
		'country_code' => '',
		'country_name' => '',
		'region_name' => '',
		'region_code' => '',
		'city' => '',
		'zippostalcode' => '',
		'latitude' => '',
		'longitude' => '',
		'timezone' => '',
		'gmtoffset' => '',
		'dstoffset' => '',
	);;
}

function old_way($ip) {

	$d = file_get_contents("http://www.ipinfodb.com/ip_query.php?ip=$ip&output=xml");

	//Use backup server if cannot make a connection
	if (!$d) {
		$backup = file_get_contents("http://backup.ipinfodb.com/ip_query.php?ip=$ip&output=xml");
		$answer = new SimpleXMLElement($backup);
		if (!$backup) {
			return FALSE;
		} // Failed to open connection
	} else {
		$answer = new SimpleXMLElement($d);
	}

	$country_code = $answer->CountryCode;
	$country_name = $answer->CountryName;
	$region_code = $answer->RegionCode;
	$region_name = $answer->RegionName;
	$city = $answer->City;
	$zippostalcode = $answer->ZipPostalCode;
	$latitude = $answer->Latitude;
	$longitude = $answer->Longitude;
	$timezone = $answer->Timezone;
	$gmtoffset = $answer->Gmtoffset;
	$dstoffset = $answer->Dstoffset;

	//Return the data as an array
	return array(
		'ip' => $ip,
		'country_code' => $country_code,
		'country_name' => $country_name,
		'region_name' => $region_name,
		'region_code' => $region_code,
		'city' => $city,
		'zippostalcode' => $zippostalcode,
		'latitude' => $latitude,
		'longitude' => $longitude,
		'timezone' => $timezone,
		'gmtoffset' => $gmtoffset,
		'dstoffset' => $dstoffset,
	);
}

function locateBrowser($agent = NULL) {

	$known = array(
		'msie',
		'firefox',
		'safari',
		'webkit',
		'opera',
		'netscape',
		'konqueror',
		'gecko',
	);
	$agent = strtolower($agent ? $agent : $_SERVER['HTTP_USER_AGENT']);
	$pattern = '#(?<browser>' . join('|', $known) . ')[/ ]+(?<version>[0-9]+(?:\.[0-9]+)?)#';
	if (!preg_match_all($pattern, $agent, $matches)) {
		return array();
	}
	$i = count($matches['browser']) - 1;

	return $matches['browser'][$i] . ' ' . $matches['version'][$i];
}

function whois_info() {

	$wresults = shell_exec("whois " . $_SERVER['REMOTE_ADDR']);
	$fs = strpos($wresults, "City: ", 0);
	if ($fs == 0) {
		$city = '';
	} else {
		$ls = strpos($wresults, "\n", $fs);
		$results = substr($wresults, $fs, $ls - $fs);
		$crap = array(
			"City:",
			"  ",
		);
		$city = str_replace($crap, "", $results);
	}
	$fs = strpos($wresults, "StateProv: ", 0);
	if ($fs == 0) {
		$state = '';
	} else {
		$ls = strpos($wresults, "\n", $fs);
		$results = substr($wresults, $fs, $ls - $fs);
		$crap = array(
			"StateProv:",
			"  ",
		);
		$state = str_replace($crap, "", $results);
	}

	$fs = strpos($wresults, "Country: ", 0);
	if ($fs == 0) {
		$country = '';
	} else {
		$ls = strpos($wresults, "\n", $fs);
		$results = substr($wresults, $fs, $ls - $fs);
		$crap = array(
			"Country:",
			"  ",
		);
		$country = str_replace($crap, "", $results);
	}
	$results = $city . " " . $state . " " . $country;

	return $results;
}

function print_long_line($string, $offset, $line, $color, $im) {

	$string = $string . ' ';
	$slen = strlen($string);
	if ($slen > 33) {
		$splitspace = strpos($string, " ", 33);
		imagestring($im, 1, $offset, $line, substr($string, 0, $splitspace), $color);
		print_long_line(substr($string, $splitspace, $slen), $offset, $line + 10, $color, $im);
	} else {
		imagestring($im, 1, $offset, $line, $string, $color);
	}
}

function find_os() {

	$browserarray = explode("; ", $_SERVER['HTTP_USER_AGENT']);
	$os = $browserarray[2];

	return $os;
}

function find_browser() {

	$browserarray = explode("; ", $_SERVER['HTTP_USER_AGENT']);
	if ($browserarray[1] == "U") {
		$browser = $browserarray[3] . $browserarray[4];
	} else {
		$browser = $browserarray[1];
	}

	return $browser;
}

?>