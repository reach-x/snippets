<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 11/28/17
 * Time: 12:45
 */


$master = file($argv[1]);
$master_counter = 0;
$master_total = count($master);
printf("loaded master file with %d lines\n", $master_total);


$slave = file($argv[2]);
$slave_counter = 0;
$slave_total = count($slave);
printf("loaded slave file with %d lines\n", $slave_total);

$matches = array();

foreach ($master as $master_line) {

	$master_counter++;
	$master_line = trim($master_line);

	if ($master_counter % 100 == 0) {
		printf("\nMaster: %0.2f (%d/%d):%d\n", ($master_counter / $master_total), $master_counter, $master_total, $match_counter);
	} else {
		printf(".");
	}

	$slave_counter = 0;
	$match_counter = 0;

	foreach ($slave as $slave_line) {

		$slave_counter++;
		$slave_line = trim($slave_line);

		if (strcasecmp($master_line, $slave_line) == 0) {
			$matches[] = $master_line;
			$match_counter++;
		}
		//
		//		if ($slave_counter % 50000 == 0) {
		//			printf("\nSlave: %0.2f (%d/%d):%d ", ($slave_counter / $slave_total), $slave_counter, $slave_total, $match_counter);
		//		} else {
		//			if ($slave_counter % 1000 == 0) {
		//				printf(".");
		//			}
		//		}
	}
}

file_put_contents($argv[3], join("\n", $matches));

