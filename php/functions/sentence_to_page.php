<?php


$sentence = "The lazy brown fox jumps over the river.";

$page = sprintf("%s.html",str_replace(" ","_",strtolower(preg_replace("/[^A-Za-z0-9 ]/", '',$sentence))));

print_r(array(
	'page'=>$page
));
