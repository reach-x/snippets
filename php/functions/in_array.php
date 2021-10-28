<?php

$zip_to_search = "00602";

$zip_codes = array(
	"00544",
	"00601",
	"00602",
	"00603",
	"00604",
	"00605",
	"00606",
	"00610",
	"00611",
	"00612",
);

if (in_array($zip_to_search, $zip_codes))
{
	print json_encode(array("result" => TRUE));
} else
{
	print json_encode(array("result" => FALSE));
}