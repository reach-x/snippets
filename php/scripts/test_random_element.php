<?php

$sample_data = array(
	"one" => 50,
	"two" => 50,
);


$counter = 0;
$tests = 1000000;
$results = array(
	"one" => 0,
	"two" => 0,
);

while ($counter <= $tests){
 $result = get_random_weighted_element($sample_data);
 $results[$result]++;
 $counter++;
}

print_r($results);

function get_random_weighted_element ($weighted_values) {

	$rand = mt_rand(1, (int) array_sum($weighted_values));

	foreach ($weighted_values as $key => $value) {
		$rand -= $value;
		if ($rand <= 0) {
			return $key;
		}
	}
}
