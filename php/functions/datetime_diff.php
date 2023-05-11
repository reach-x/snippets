<?php

date_default_timezone_set("US/Pacific");

$current_time = date("Hi");

//$now_time = DateTime::createFromFormat("Hi", $now);
//$then = new DateTime("2020-01-01 12:34:56");

print_r(array(
	'current_time'       => $current_time,
	//'now_time' => $now_time,
	//'then'      => $then,
	//'diff'      => DateTime::diff($now, $then),
));

