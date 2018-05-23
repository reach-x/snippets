<?php

$tests = array(
	'"zero"' => "0",
	'zero' => 0,
	'zero.zero' => '0.0',
	'double quotes' => "",
	'null' => NULL,
	'false' => FALSE,
	'array()' => array(),
	'$var' => $var,
	'1' => 1,
);

foreach ($tests as $name => $value){
	printf("empty(%s):%s\n",$name,empty($value) ? "TRUE" : "FALSE");
}

