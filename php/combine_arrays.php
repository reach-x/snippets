<?php

$keys = array(
	"one",
	"two",
	"three",
);

$values = array(
	"one_value",
	"two_value",
	"three_value",
);


print_r(combine_keys_and_values($keys,$values));

function combine_keys_and_values($keys,$values){

	$combined = array();

	for($index=0;$index<count($keys);$index++){
		$combined[$keys[$index]] = $values[$index];
	}

	return $combined;
}
