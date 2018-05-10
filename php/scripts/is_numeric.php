<?php

print_r(
	array(
		'argv' => $argv[1],
		'is_numeric' => is_numeric($argv[1]) ? "true" : "false"
	)
);
