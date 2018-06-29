<?php


$string = $argv[1];
$string = stripcslashes($string);

$string= stripcslashes($string);

if (preg_match("/\?/", $string)){
	$parts = explode("?", $string);
	$string = $parts[1];
} else {
	printf("no question mark\n");
}

$parameters = array();

parse_str($string,$parameters);

print_r(array(
	'string' => $string,
	'parse_str' => $parameters,	
));
