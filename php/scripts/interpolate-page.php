<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 2019-02-12
 * Time: 14:16
 */


$pairs = array(
	'HIT_ID' => 736171123,
	'SID' => 7817,
	'C1' => 'RM_1404_7',
	'C2' => '15182a0mu9vb',
	'C3' => 'Ad-1',
	'TRACKING_TOKEN_ID' => 100156606,
	'AFFILIATE_ID' => 12345,
);

$content = file_get_contents($argv[1]);

echo interpolate($pairs, $content);


function interpolate($pairs, $content) {

	foreach ($pairs as $key => $value) {

		$search = sprintf("%%%%%s%%%%", $key);

		$content = str_replace($search, $value, $content);

	}

	return $content;
}
