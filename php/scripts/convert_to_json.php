<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 4/6/17
 * Time: 10:35
 */


//printf("%s\n", json_encode(file($argv[1])));

$output = array();

foreach (file($argv[1]) as $line) {
	$output[] = trim($line);

}
printf("%s\n", json_encode($output));