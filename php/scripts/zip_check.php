<?php

$zip_to_search = trim(filter_var($HTTP_GET_VARS['zip_code'], FILTER_SANITIZE_STRING));
$zip_codes = file("zip_codes.csv");
$has_zip = FALSE;

foreach ($zip_codes as $zip_code)
{
	$zip_code = trim($zip_code);

	if ($zip_to_search == $zip_code)
	{
		$has_zip = TRUE;
	}
}

print json_encode(array("result" => $has_zip));
