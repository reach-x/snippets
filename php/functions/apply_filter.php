<?php

$field_name = "IS_VALID_LEAD";

$value = "invalid";

$criteria = array(
	'IS_VALID_LEAD' => array("invalid"),
);

$is_filtered = FALSE;

if (isset($criteria[$field_name])) {
	if (is_array($criteria[$field_name])) {
		$is_filtered = intval(!(in_array($value, $criteria[$field_name])));
	}
}
