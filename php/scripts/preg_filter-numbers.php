<?php


print_r(
	array(
		'argv' => $argv[1],
		'result' => preg_filter('/[^0-9]/','',$argv[1]),
	)
);

