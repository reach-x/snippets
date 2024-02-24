<?php

// https://display.ringba.com/v2/nis/gn/

$test_tag = new stdClass();
$test_tag->type = "User";
$test_tag->transaction_id = "ASDFDSAFDSA";
$post_data = new stdClass();
$post_data->JsTagId = "JS5cbe8d114f0844138568fc32c608b7a7";
$post_data->CurrentEpoch = time();
$post_data->Tags = array(
	$test_tag,
);

//printf("%s\n",json_encode($post_data));
//exit;

$curl = curl_init();

curl_setopt_array($curl, array(
	CURLOPT_URL            => 'https://display.ringba.com/v2/nis/gn/',
	CURLOPT_RETURNTRANSFER => TRUE,
	CURLOPT_POST           => TRUE,
	CURLOPT_ENCODING       => '',
	CURLOPT_MAXREDIRS      => 10,
	CURLOPT_TIMEOUT        => 0,
	CURLOPT_FOLLOWLOCATION => TRUE,
	CURLOPT_HTTP_VERSION   => CURL_HTTP_VERSION_1_1,
	CURLOPT_CUSTOMREQUEST  => 'POST',
	CURLOPT_POSTFIELDS     => json_encode($post_data),
	CURLOPT_HTTPHEADER     => array(
		'Content-Type: application/json',
	),
));

$response = curl_exec($curl);

curl_close($curl);
echo $response;
