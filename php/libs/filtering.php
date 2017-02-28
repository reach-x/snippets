<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 2/22/17
 * Time: 16:43
 *
 * $request
 *
 * @param $request - incoming $_REQUEST or any key value pairs to be filtered
 * @param $fields - array of incoming field data and how it should be typed
 *
 * each $field should have this structure
 *  $field = [
 *      'field_name' => string,
 *      'data_size' => integer,
 *      'is_required' => boolean,
 *      'filters' => comma separated filters for filter_var,
 *      'data_type' => type of data i.e. VARCHAR, DATE, INT, IPV4...
 * ];
 *
 * @return array
 */
$fields = [
	[
		'field_name' => "VARCHAR_GOOD",
		'data_size' => 255,
		'is_required' => 1,
		'filters' => "FILTER_SANITIZE_STRING",
		'data_type' => "VARCHAR",

	],
	[
		'field_name' => "VARCHAR_BAD",
		'data_size' => 255,
		'is_required' => 1,
		'filters' => "FILTER_SANITIZE_STRING",
		'data_type' => "VARCHAR",

	],
	[
		'field_name' => "IPV4_GOOD",
		'data_size' => 16,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "IPV4",

	],
	[
		'field_name' => "IPV4_BAD",
		'data_size' => 16,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "IPV4",
	],
	[
		'field_name' => "BOOL_GOOD",
		'data_size' => 5,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "BOOL",
	],
	[
		'field_name' => "BOOL_BAD",
		'data_size' => 5,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "BOOL",
	],
	[
		'field_name' => "INT_GOOD",
		'data_size' => 10,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "INT",
	],
	[
		'field_name' => "INT_BAD",
		'data_size' => 10,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "INT",
	],
	[
		'field_name' => "FLOAT_GOOD",
		'data_size' => 10,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "FLOAT",
	],
	[
		'field_name' => "FLOAT_BAD",
		'data_size' => 10,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "FLOAT",
	],
	[
		'field_name' => "PHONE_GOOD",
		'data_size' => 12,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "PHONE",
	],
	[
		'field_name' => "PHONE_BAD",
		'data_size' => 12,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "PHONE",
	],
	[
		'field_name' => "DATE_GOOD",
		'data_size' => 10,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "DATE",
	],
	[
		'field_name' => "DATE_BAD",
		'data_size' => 10,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "DATE",
	],
	[
		'field_name' => "DATETIME_GOOD",
		'data_size' => 19,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "DATETIME",
	],
	[
		'field_name' => "DATETIME_BAD",
		'data_size' => 19,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "DATETIME",
	],
	[
		'field_name' => "TIMESTAMP_GOOD",
		'data_size' => 10,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "TIMESTAMP",
	],
	[
		'field_name' => "TIMESTAMP_BAD",
		'data_size' => 10,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "TIMESTAMP",
	],
	[
		'field_name' => "HOSTNAME_GOOD",
		'data_size' => 255,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "HOSTNAME",
	],
	[
		'field_name' => "HOSTNAME_BAD",
		'data_size' => 255,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "HOSTNAME",
	],
	[
		'field_name' => "EMAIL_GOOD",
		'data_size' => 255,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "EMAIL",
	],
	[
		'field_name' => "EMAIL_BAD",
		'data_size' => 255,
		'is_required' => 1,
		'filters' => "",
		'data_type' => "EMAIL",
	],
];

$request = [
	'VARCHAR_GOOD' => "VARCHAR",
	'VARCHAR_BAD' => "ASDFGHIJKLMNOPQRSTUVWXYZASDFGHIJKLMNOPQRSTUVWXYZASDFGHIJKLMNOPQRSTUVWXYZASDFGHIJKLMNOPQRSTUVWXYZASDFGHIJKLMNOPQRSTUVWXYZASDFGHIJKLMNOPQRSTUVWXYZASDFGHIJKLMNOPQRSTUVWXYZASDFGHIJKLMNOPQRSTUVWXYZASDFGHIJKLMNOPQRSTUVWXYZASDFGHIJKLMNOPQRSTUVWXYZASDFGHIJKLMNOPQRSTUVWXYZ",
	"IPV4_GOOD" => "1.2.3.4",
	"IPV4_BAD" => "192.168.1.1",
	"BOOL_GOOD" => "TRUE",
	"BOOL_BAD" => "Whatever",
	"INT_GOOD" => 1134,
	"INT_BAD" => "asdf",
	"FLOAT_GOOD" => 0.123,
	"FLOAT_BAD" => "ASDF",
	"PHONE_GOOD" => "206-939-1981",
	"PHONE_BAD" => "XXX-555-5309",
	"DATE_GOOD" => "2017-01-01",
	"DATE_BAD" => "March 1st",
	"DATETIME_GOOD" => "2017-01-01 12:00:00",
	"DATETIME_BAD" => "About then",
	"TIMESTAMP_GOOD" => "1487812722",
	"TIMESTAMP_BAD" => "ASDF",
	"HOSTNAME_GOOD" => "http://www.google.com/",
	"HOSTNAME_BAD" => "google.com/asdf/fdsa/as?asdf=2342",
	"EMAIL_GOOD" => "no@sense.com",
	"EMAIL_BAD" => "asdf.com",
];

$filter = new filter();

print_r($filter->run($request, $fields));


class filter {

	/**
	 * @param $request
	 * @param $fields
	 *
	 * @return array
	 */
	function run($request, $fields) {

		$clean_data = array();
		$validation_errors = array();

		// iterate through wanted fields
		foreach ($fields as $field) {

			$value = "";

			if (isset($request[$field['field_name']])) {

				$value = filter_var($request[$field['field_name']], FILTER_SANITIZE_STRING);

				if (empty($value) && $field['is_required']) {
					$validation_errors[] = sprintf("%s has no value.", $field['field_name']);
					continue;
				}

			} else {
				if ($field['is_required']) {
					$validation_errors[] = sprintf("%s is not passed via POST CGI.", $field['field_name']);
					continue;
				}
			}

			if ($field['data_size'] > 0 && strlen($value) > $field['data_size']) {
				$validation_errors[] = sprintf("%s value is longer than %s", $field['field_name'], $field['data_size']);
				continue;
			}

			// decompress multibyte characters
			$value = preg_replace_callback("/(&#[0-9]+;)/", function($string) {

				return mb_convert_encoding($string[1], "UTF-8", "HTML-ENTITIES");
			}, $value);

			// remove residual character encoding
			$value = rawurldecode($value);


			foreach (explode(",", $field['filters']) as $filter) {

				if (!empty($filter)) {

					$constant = constant($filter);

					if (!empty($constant)) {
						$value = filter_var($value, $constant);
					}
				}
			}

			switch ($field['data_type']) {

				case "VARCHAR":
					$clean_data[$field['field_name']] = filter_var($value, FILTER_SANITIZE_STRING);
					break;

				case "IPV4":

					if (filter_var($value, FILTER_VALIDATE_IP, FILTER_FLAG_IPV4 | FILTER_FLAG_NO_PRIV_RANGE)) {
						$clean_data[$field['field_name']] = $value;
					} else {
						if ($field['is_required']) {
							$validation_errors[] = sprintf("%s is not a valid ip address $value", $field['field_name']);
						}
					}
					break;

				case "BOOL":
					//TODO: should have a filter_var BOOLEAN type here to accept multiple types of boolean values.
					$clean_data[$field['field_name']] = ($value == TRUE ? 1 : 0);
					break;

				case "INT":
					if (filter_var($value, FILTER_VALIDATE_INT)) {
						$clean_data[$field['field_name']] = filter_var($value, FILTER_SANITIZE_NUMBER_INT);
					} else {
						if ($field['is_required']) {
							$validation_errors[] = sprintf("%s value is not numeric: $value", $field['field_name']);
						}
					}
					break;

				case "FLOAT":
					if (filter_var($value, FILTER_VALIDATE_FLOAT)) {
						$clean_data[$field['field_name']] = filter_var($value, FILTER_SANITIZE_NUMBER_FLOAT);
					} else {
						if ($field['is_required']) {
							$validation_errors[] = sprintf("%s value is not a float: %s", $field['field_name'], $value);
						}
					}
					break;

				case "PHONE":

					$value = str_replace(array(
						'(',
						')',
						' ',
						'-',
						'.',
					), '', $value);

					$clean_data[$field['field_name']] = preg_replace('/(\s|\(|\)|[^0-9])/', '', $value);

					if (empty($value) && $field['is_required'] == 1) {
						$validation_errors[] = sprintf("%s is required but empty.", $field['field_name']);
					} elseif (!is_numeric($value)) {
						$validation_errors[] = sprintf("%s is not numeric.", $field['field_name']);
					}

					break;

				case "DATE":
					if (preg_match("/^\d{4}\-(0?[1-9]|1[012])\-(0?[1-9]|[12][0-9]|3[01])$/", $value)) {
						$date = $this->parse_date($value);
						print_r($date[0]);
						$clean_data[$field['field_name']] = date("Y-m-d", strtotime($date[0]->date));
					} else {
						if ($field['is_required']) {
							$validation_errors[] = sprintf("%s is not a valid date format YYYY-MM-DD.", $field['field_name']);
						}
					}
					break;

				case "DATETIME":
					if (strtotime($value)) {
						$clean_data[$field['field_name']] = date("Y-m-d H:i:s", strtotime($value));
					} else {
						if ($field['is_required']) {
							$validation_errors[] = sprintf("%s is not a valid datetime format YYYY-MM-DD HH:MM:SS.", $field['field_name']);
						}
					}
					break;

				case "TIMESTAMP":
					if (is_numeric($value)) {
						$clean_data[$field['field_name']] = date("Y-m-d", $value);
					} else {
						if ($field['is_required']) {
							$validation_errors[] = sprintf("%s is not a valid timestamp format.", $field['field_name']);
						}
					}
					break;

				case "HOSTNAME":

					$parsed_url = parse_url($value);

					if (isset($parsed_url["scheme"]) && isset($parsed_url["host"])) {
						$clean_data[$field['field_name']] = sprintf("%s://%s", $parsed_url["scheme"], $parsed_url["host"]);
					} else {
						if ($field['is_required']) {
							$validation_errors[] = sprintf("%s is not a valid source hostname without scheme and hostname", $field['field_name']);
						}
					}
					break;

				case "EMAIL":

					if (filter_var($value, FILTER_VALIDATE_EMAIL)) {
						$clean_data[$field['field_name']] = $value;
					} else {
						if ($field['is_required']) {
							$validation_errors[] = sprintf("%s is not a valid email address.", $field['field_name']);
						}
					}
					break;

				default:

					if (!empty($value)) {
						$clean_data[$field['field_name']] = $value;
					} else {
						if ($field['is_required']) {
							$validation_errors[] = sprintf("%s is required and missing.", $field['field_name']);
						}
					}
					break;
			}
		}

		return [
			$clean_data,
			$validation_errors,
		];
	}

	function parse_date($potential_date) {

		$formats = array(
			"m.d.Y",
			"m/d/Y",
			"Ymd",
			"Y-m-d",
			"//Y",
			"Y",
		); // and so on.....
		$date = "";

		foreach ($formats as $format) {
			$date = DateTime::createFromFormat($format, $potential_date);

			if ($date != FALSE) {
				break;
			}
		}

		return array($date);

	}
}