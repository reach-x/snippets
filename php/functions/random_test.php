<?php

printf("%10s%10s%10s%10s%10s\n", 'first', 'second', 'third', 'fourth', 'fifth');

$weighted_values = array(
	"first" => 10,
	"second" => 10,
	"third" => 10,
	"fourth" => 10,
	"fifth" => 10,
);

$loop_counter = 0;

while ($loop_counter < 10) {

	$results = array();
	$counter = 0;

	while ($counter < 1000000) {

		$index = get_random_weighted_element($weighted_values);

		if (!isset($results[$index])) {
			$results[$index] = 1;
		} else {
			$results[$index]++;
		}
		$counter++;
	}

	printf("%10s%10s%10s%10s%10s\n", $results['first'], $results['second'], $results['third'], $results['fourth'], $results['fifth']);
	$loop_counter++;
}

function get_random_weighted_element($weighted_values) {

	$rand = mt_rand(1, (int) array_sum($weighted_values));

	foreach ($weighted_values as $key => $value) {

		$rand -= $value;

		if ($rand <= 0) {
			return $key;
		}
	}
}

