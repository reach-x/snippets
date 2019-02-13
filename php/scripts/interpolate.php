<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 2019-02-12
 * Time: 14:16
 */


$pairs = array(
	'key1' => 'value1',
	'key2' => 'value2',
);

$content = <<<EOM
This is the %%key1%% %%key2%% message 
EOM;

echo interpolate($pairs, $content);


function interpolate($pairs, $content) {

	foreach ($pairs as $key => $value) {

		$search = sprintf("%%%%%s%%%%", $key);

		$content = str_replace($search, $value, $content);

	}

	return $content;
}
