<?php
date_default_timezone_set("US/Pacific");

print_r(
	array(
		'$argv' => $argv,
		'strtotime' => strtotime($argv[1]),
		'date' => date("H:i:s m/d/Y",strtotime($argv[1])),
	)
);
