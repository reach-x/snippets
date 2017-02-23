<?php

$value = $argv[1];
$field_name = "isp_domain_id";
$criteria = json_decode('{"isp_domain_id":[72,83,95,2,24,15206,48,70,12843,23399,12647,18463,165,12763,19223,30,69]}', TRUE);
$is_filtered = in_array($value, $criteria[$field_name]);

print_r(array(
	'value' => $value,
	'field_name' => $field_name,
	'criteria' => $criteria,
	'is_filtered' => $is_filtered,
));
