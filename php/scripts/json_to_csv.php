<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 4/6/17
 * Time: 10:35
 */


if ($argc == 3) {

	$contents = file_get_contents($argv[1]);
	$fp = fopen($argv[2], 'w');

	$json = json_decode($contents, TRUE);

	$output = array();
	//$header = array_keys($json['records'][0]);
	$header = array_keys($json[0]);

	fputcsv($fp, $header);

	foreach ($json as $row) {

		fputcsv($fp, $row);
	}

	fclose($fp);

} else {
	printf("%s <input file> <output file>\n", $argv[0]);
}
