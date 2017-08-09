<?php
date_default_timezone_set("US/Pacific");

print_r(array(
		'$argv' => $argv,
		'date' => date("H:i:s m/d/Y", $argv[1]),
	));
