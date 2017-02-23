<?php

$month = $argv[1];
$day = $argv[2];
$year = $argv[3];

$is_valid = (strtotime("today - 14 days") < strtotime(sprintf("%s/%s/%s", $month, $day, $year)));

if ($is_valid) {
	echo "valid";
} else {
	echo "invalid";
}

printf("\n");




