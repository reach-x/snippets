<?php

$pattern = $argv[1];
$replacement = $argv[2];
$string = $argv[3];

print_r(array(
	'pattern' => $pattern,
	'replacement' => $replacement,
	'string' => $string,
	'preg_replace' => preg_replace($pattern, $replacement, $string),
));


