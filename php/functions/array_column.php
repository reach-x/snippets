<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 2/26/17
 * Time: 18:15
 */

// Array representing a possible record set returned from a database
$records = array(
	[
		'id' => 2135,
		'first_name' => 'John',
		'last_name' => 'Doe',
	],
	[
		'id' => 3245,
		'first_name' => 'Sally',
		'last_name' => 'Smith',
	],
	[
		'id' => 5342,
		//'first_name' => 'Jane', // commented out to see if array indexes are maintained and they're not.
		'last_name' => 'Jones',
	],
	[
		'id' => 5623,
		'first_name' => 'Peter',
		'last_name' => 'Doe',
	],
);

$first_names = array_column($records, 'first_name');
print_r($first_names);