<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 8/28/17
 * Time: 14:43
 */

$test_array = array(
	'space' => ' ',
	'nospace' => '',
	'null' => NULL,
	'filled' => 'has value',
);


print_r(array(
	'isset_and_not_empty(space)' => isset_and_not_empty($test_array, 'space'),
	'isset_and_not_empty(nospace)' => isset_and_not_empty($test_array, 'nospace'),
	'isset_and_not_empty(null)' => isset_and_not_empty($test_array, 'null'),
	'isset_and_not_empty(filled)' => isset_and_not_empty($test_array, 'filled'),
	'isset_and_not_empty(nonexistant)' => isset_and_not_empty($test_array, 'nonexistant'),
	'notset_or_empty(space)' => notset_or_empty($test_array, 'space'),
	'notset_or_empty(nospace)' => notset_or_empty($test_array, 'nospace'),
	'notset_or_empty(null)' => notset_or_empty($test_array, 'null'),
	'notset_or_empty(filled)' => notset_or_empty($test_array, 'filled'),
	'notset_or_empty(nonexistant)' => notset_or_empty($test_array, 'nonexistant'),
));


function isset_and_not_empty($array, $index) {

	// is the array value set
	//
	//	print_r(array(
	//		'array' => $array,
	//		'index' => $index,
	//		'isset' => isset($array[$index]) ? "is set" : "is not set",
	//		'empty' => empty($array[$index]) ? "is empty" : "is not empty",
	//	));

	return (isset($array[$index]) && !empty($array[$index]));
}

function notset_or_empty($array, $index) {

	//	print_r(array(
	//		'array' => $array,
	//		'index' => $index,
	//		'!isset' => !isset($array[$index]) ? "is set" : "is not set",
	//		'empty' => empty($array[$index]) ? "is empty" : "is not empty",
	//	));


	return (!isset($array[$index]) || empty($array[$index]));
}
