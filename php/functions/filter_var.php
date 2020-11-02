<?php


$string="271273_";

print_r(array(
	'string'=> $string,
	'filter_var'=>filter_var($string,FILTER_SANITIZE_STRING|FILTER_SANITIZE_NUMBER_INT)
));
