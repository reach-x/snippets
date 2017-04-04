<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 3/28/17
 * Time: 11:39
 */

if ($argc == 3) {
	$value = $argv[1];
	$values = $argv[2];
	$field_name = "field_name";
} else {
	die("Need values. ./$argv[0] value 'array' ");
}

//{"BUYER_ID": [10001,10002]}

$json = sprintf("{\"field_name\":[%s]}", $values);
$criteria = json_decode($json, TRUE);

if (json_last_error()) {
	die("JSON: " . json_last_error_msg());
} else {

	if (is_array($criteria[$field_name])) {
		$record_conversion = in_array($value, $criteria[$field_name]);
	} else {
		$record_conversion = FALSE;
	}

	print_r([
		'criteria' => $criteria,
		'field_name' => $criteria[$field_name],
		'json' => $json,
		'record_conversion' => $record_conversion == TRUE ? "TRUE" : "FALSE",
	]);
}



$criteria = json_decode($criteria, TRUE);

if (is_array($criteria[$field_name])) {
	$record_conversion = in_array($value, $criteria[$field_name]);
} else {
	$record_conversion = FALSE;
}

$this->write_log(json_encode([
	'site_id' => $site_id,
	'filter' => 'SITE_ENTRY_POST_FILTER_IN_ARRAY',
	'value' => $value,
	'criteria' => $criteria,
	'record_conversion' => $record_conversion,
	'$site_entry_data' => $site_entry_data,
	'method' => '$record_conversion = in_array($value, $criteria[$field_name]);',
	'expanded' => 'in_array(' . $value . ',' . $criteria[$field_name] . ');',

]));

$reasons['SITE_ENTRY_POST_FILTER_IN_ARRAY'] = $record_conversion;

