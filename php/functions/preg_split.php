<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 2/24/17
 * Time: 12:18
 */

//$single_value_string = "asdf";
//$multiple_value_string = "asdf1,asdf2,asdf3,asdf4,asdf5,asdf6";
//print_r([
//	'$single_value_string' => preg_split("/,/", $single_value_string),
//	'$multiple_value_string' => preg_split("/,/", $multiple_value_string),
//]);

$command_strings = [
	"JOB_POST",
	"CBS_POST",
	"LBC_POST",
	"LBC_AND_CBS_POST",
	"LAS_POST",
	"LAS_AND_LBC_POST",
	"MULTIPLE_COREG_POSTS",
	"MULTIPLE_COREG_AND_LBC_POSTS",
	"LDS_POST",
	"LDS_POST",
];


foreach ($command_strings as $commands) {

	$original_string = $commands;

	$commands = preg_replace("/_POSTS?|_AND/", ",", $commands);
	$commands = preg_replace("/_/", "", $commands);
	$commands = preg_replace("/,$/", "", $commands);

	$commands = preg_split("/,/", $commands);


	print_r([
		'$original_string' => $original_string,
		'commands' => $commands,
	]);
}


