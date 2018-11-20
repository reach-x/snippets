<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 8/9/17
 * Time: 14:24
 */


if ($argc != 3) {

	printf("%s <filename> 'variable to eval'\n", $argv[0]);

} else {
	//	printf("ARGC is %s\nARGV: %s\n", $argc,print_r($argv,TRUE));

	$object = json_decode(file_get_contents($argv[1]),TRUE);
	
	if (isset($object[$argv[2]])){
		printf("%s\n",$object[$argv[2]]);
	} else {
		printf("Element not found: %s\n",$argv[2]);
		print_r($object);
	}
}
