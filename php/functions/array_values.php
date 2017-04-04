<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 4/4/17
 * Time: 08:37
 */


$fields = [
	'FIRST_NAME' => 1,
	'LAST_NAME' => 2,
	'IP_ADDRESS' => 12,
	'GENERATED_AT' => 13,
	'SOURCE_DOMAIN' => 16,
];

print_r(join(",",array_values($fields)));