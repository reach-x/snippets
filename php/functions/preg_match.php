<?php

$pattern = $argv[1];
$string = $argv[2];

print_r(array(
	'pattern' => $pattern,
	'string' => $string,
	'return value preg_match' => preg_match($pattern, $string),
));


