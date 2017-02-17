<?php


$test = array(
	'asdf%20asdf',
	'asdf asdf',
	'asdf%7Casdf',
);


print_r(array_map("urldecode", $test));
