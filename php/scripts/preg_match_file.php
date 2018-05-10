#!/usr/local/bin/php
<?php

$matches = array();
$file_contents = file_get_contents($argv[2]);

print_r(array(
	'results' => preg_match($argv[1], $file_contents, $matches),
)
);

printf("\n");

print_r($matches);

