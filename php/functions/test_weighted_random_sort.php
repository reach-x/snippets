<?php
/**
 * Created by PhpStorm.
 * User: tlai
 * Date: 2/24/17
 * Time: 9:49 AM
 */

$results = array();

for ($i = 1; $i <= 500; $i++) {

	require('./weighted_random_sort.php');

//	print_r($weighted_random_sort);

	$j = 1;

	foreach ($weighted_random_sort as $value) {
		$results[$j][] = $value;
		$j++;
	}

}

//print_r($results);

foreach ($results as $key => $result) {
	echo "$key Place Queue\n";
	print_r(array_count_values($result));
}