<?php
/**
 * Created by PhpStorm.
 * User: tlai
 * Date: 2/24/17
 * Time: 8:46 AM
 */

$bag_o_digits = range(1, 100);

$weights = array("0" => array('range' => [1, 20]),
                 "1" => array('range' => [21, 40]),
                 "2" => array('range' => [41, 60]),
                 "3" => array('range' => [61, 100]),
);

shuffle($bag_o_digits);

//print_r($bag_o_digits);

$weight_lookup = function ($digit) use($weights) {
					$test_func = function($weight) use($digit) {
						return filter_var($digit, FILTER_VALIDATE_INT, array('options' => array('min_range' => $weight['range'][0], 'max_range' => $weight['range'][1])));
					};
					return key(array_filter($weights, $test_func));
				};

$bag_o_digits = array_map($weight_lookup, $bag_o_digits);

//print_r($bag_o_digits);

$weighted_random_sort = array_flip(array_map(function($key) use($bag_o_digits) { return array_search($key, $bag_o_digits); }, array_keys($weights)));

ksort($weighted_random_sort);

//print_r($weighted_random_sort);

return $weighted_random_sort;

?>